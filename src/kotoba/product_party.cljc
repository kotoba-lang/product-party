(ns kotoba.product-party
  "Portable product ↔ party (company/supplier/merchant) join graph.

  This library is the **operator-facing join contract** between trade items
  and legal/operating parties for kotoba-lang + cloud-itonami. It deliberately
  does NOT own product identity or company master data:

    - Product identity SSoT  → etzhayyim GTIN actor / uchiwake product graph
    - Listed-company SSoT    → etzhayyim kabuto (`org.corp.*`)
    - Commodity classification → etzhayyim `20-actors/unspsc` (8-digit codes)
    - Segment blueprints     → kotoba-lang/unspsc (2-digit segments)
    - Industry blueprints    → kotoba-lang/industry (ISIC classes)

  What this lib owns is the **edge**: which party plays which role on which
  product, with UNSPSC/ISIC tags for procurement and open-business routing.

  Edge vocabulary mirrors uchiwake's brand-owner / supplier / carrier /
  operator links and kabuto's `org.corp.*` id space, so operator tenants can
  project those graphs without re-implementing them.

  Pure data + pure functions. No network, no governor, no store backend.
  cloud-itonami wraps this with a tenant MemStore + bind/revoke effects."
  (:require [clojure.string :as str]))

;; ───────────────────────── role vocabulary ─────────────────────────

(def roles
  "Closed set of product↔party relationship roles.
   :brand-owner  — legal brand owner (kabuto brand-owner)
   :manufacturer — physical maker / contract manufacturer
   :assembler    — final assembly (e.g. EMS / Foxconn-class)
   :supplier     — parts / materials supplier on the product BOM
   :distributor  — wholesale distributor of the finished good
   :merchant     — retail EC seller (kakaku merchant apex, not brand owner)
   :carrier      — logistics leg carrier
   :operator     — process-step operator / service operator on the product"
  #{:brand-owner :manufacturer :assembler :supplier
    :distributor :merchant :carrier :operator})

(def high-stakes-roles
  "Role changes that an operator governor should human-gate by default."
  #{:brand-owner :manufacturer})

(def sourcing-values
  "Honesty tags, same closed set as uchiwake/kabuto ontologies."
  #{:authoritative :representative :synthesized})

;; ───────────────────────── id helpers ─────────────────────────

(defn digits-only
  "Strip non-digits from a GTIN-ish string."
  [s]
  (when s
    (str/replace (str s) #"[^0-9]" "")))

(defn normalize-gtin
  "Normalize a GTIN-8/12/13/14 string to left-zero-padded GTIN-14 digits.
   Returns nil when the digit length is not one of {8,12,13,14}."
  [gtin]
  (let [d (digits-only gtin)]
    (when (and d (#{"8" "12" "13" "14"} (str (count d))))
      (str (apply str (repeat (- 14 (count d)) "0")) d))))

(defn gtin-check-digit
  "GS1 mod-10 check digit for the first 13 digits of a GTIN-14 body
   (or for 7/11/12 digit bodies of shorter forms). Returns an int 0-9,
   or nil when input is not pure digits of a supported body length."
  [body-digits]
  (let [d (digits-only body-digits)]
    (when (and d (#{"7" "11" "12" "13"} (str (count d))))
      (let [weights (cycle [3 1])
            ;; GS1 weights from the RIGHT: odd positions (1-based from right) weight 3
            rev (reverse (map #(- (int %) (int \0)) d))
            s (reduce + (map * rev weights))]
        (mod (- 10 (mod s 10)) 10)))))

(defn valid-gtin?
  "True when `gtin` normalizes to GTIN-14 and its check digit matches."
  [gtin]
  (let [n (normalize-gtin gtin)]
    (boolean
     (when n
       (let [body (subs n 0 13)
             expected (gtin-check-digit body)
             actual (- (int (nth n 13)) (int \0))]
         (= expected actual))))))

(defn product-id
  "Canonical product id: `gtin.<14>` when a GTIN is known, else `prod.<slug>`."
  ([x]
   (cond
     (map? x)
     (or (:product/id x)
         (when-let [g (or (:product/gtin x) (:gtin x))]
           (product-id g))
         (when-let [slug (or (:product/slug x) (:slug x) (:name x))]
           (product-id nil slug)))
     (string? x)
     (cond
       (normalize-gtin x) (str "gtin." (normalize-gtin x))
       (or (str/starts-with? x "prod.")
           (str/starts-with? x "gtin.")
           (str/starts-with? x "int.")) x
       (and (seq x) (not (str/includes? x " ")))
       (product-id nil x)
       :else nil)
     :else nil))
  ([_gtin slug]
   (str "prod." (-> (str slug)
                    str/lower-case
                    (str/replace #"[^a-z0-9._-]+" "-")
                    (str/replace #"^-+|-+$" "")))))

(defn valid-product-id?
  [id]
  (boolean
   (when (string? id)
     (or (and (str/starts-with? id "gtin.")
              (valid-gtin? (subs id 5)))
         (and (str/starts-with? id "prod.")
              (re-matches #"prod\.[a-z0-9][a-z0-9._-]*" id))
         (and (str/starts-with? id "int.")
              (re-matches #"int\.[a-z0-9][a-z0-9._-]*" id))))))

(defn valid-party-id?
  "Accepts kabuto-style `org.corp.<cc>.<slug>`, supplier-style `sup-<id>`,
   merchant apex `merchant.<apex>`, or did:web party refs."
  [id]
  (boolean
   (when (string? id)
     (or (re-matches #"org\.corp\.[a-z]{2}\.[a-z0-9][a-z0-9._-]*" id)
         (re-matches #"sup-[a-z0-9][a-z0-9._-]*" id)
         (re-matches #"merchant\.[a-z0-9][a-z0-9._-]*" id)
         (str/starts-with? id "did:web:")))))

(defn unspsc-segment
  "Extract the 2-digit UNSPSC segment from an 8-digit (or longer) commodity
   code, or pass through an already-2-digit segment. Returns nil if unusable."
  [code]
  (let [d (digits-only code)]
    (cond
      (and d (= 2 (count d))) d
      (and d (>= (count d) 2)) (subs d 0 2)
      :else nil)))

(defn valid-unspsc-commodity?
  "8-digit commodity code (UNSPSC leaf commonly used in this workspace)."
  [code]
  (boolean (re-matches #"\d{8}" (or (digits-only code) ""))))

(defn valid-isic?
  "ISIC Rev.4 class pattern used by cloud-itonami registry (2–4 digits)."
  [code]
  (boolean (re-matches #"\d{2,4}" (str code))))

;; ───────────────────────── constructors ─────────────────────────

(defn party
  "Build a party (company / supplier / merchant) projection.
   Does not claim to be the kabuto SSoT — a portable reference record."
  [{:keys [id name kind isic country sourcing]
    unspsc-seg :unspsc-segment
    :or {kind :company sourcing :representative}}]
  (let [id (str id)
        seg (some-> unspsc-seg unspsc-segment)]
    (when-not (valid-party-id? id)
      (throw (ex-info "invalid party id"
                      {:id id :hint "org.corp.<cc>.<slug> | sup-<id> | merchant.<apex> | did:web:..."})))
    (when-not (#{:company :supplier :merchant :operator :carrier} kind)
      (throw (ex-info "invalid party kind" {:kind kind})))
    (when (and isic (not (valid-isic? isic)))
      (throw (ex-info "invalid ISIC" {:isic isic})))
    (when (and sourcing (not (sourcing-values sourcing)))
      (throw (ex-info "invalid sourcing" {:sourcing sourcing})))
    (cond-> {:party/id id
             :party/kind kind
             :party/sourcing sourcing}
      name (assoc :party/name name)
      isic (assoc :party/isic (str isic))
      country (assoc :party/country country)
      seg (assoc :party/unspsc-segment seg))))

(defn product
  "Build a trade-item projection. Prefer GTIN-keyed ids when a GTIN is known."
  [{:keys [id gtin name brand unspsc hs-code sector sourcing]
    :or {sourcing :representative}}]
  (let [norm-gtin (some-> gtin normalize-gtin)
        id (or id
               (when norm-gtin (str "gtin." norm-gtin))
               (throw (ex-info "product requires :id or :gtin" {})))]
    (when-not (valid-product-id? id)
      (throw (ex-info "invalid product id" {:id id})))
    (when (and norm-gtin (not (valid-gtin? norm-gtin)))
      (throw (ex-info "GTIN check digit failed" {:gtin norm-gtin})))
    (when (and unspsc (not (or (valid-unspsc-commodity? unspsc)
                               (= 2 (count (or (digits-only unspsc) ""))))))
      (throw (ex-info "invalid UNSPSC" {:unspsc unspsc})))
    (when (and sourcing (not (sourcing-values sourcing)))
      (throw (ex-info "invalid sourcing" {:sourcing sourcing})))
    (cond-> {:product/id id
             :product/sourcing sourcing}
      norm-gtin (assoc :product/gtin norm-gtin)
      name (assoc :product/name name)
      brand (assoc :product/brand brand)
      unspsc (assoc :product/unspsc (digits-only unspsc))
      hs-code (assoc :product/hs-code (str hs-code))
      sector (assoc :product/sector sector))))

(defn edge-id
  "Stable edge id from product + role + party."
  [product-id role party-id]
  (str "pp." product-id "." (name role) "." party-id))

(defn edge
  "Build a party-product edge. Role must be in `roles`."
  [{:keys [id product party role unspsc sourcing status note]
    :or {sourcing :representative status :active}}]
  (let [product (str product)
        party (str party)
        role (keyword role)
        id (or id (edge-id product role party))]
    (when-not (valid-product-id? product)
      (throw (ex-info "invalid product id on edge" {:product product})))
    (when-not (valid-party-id? party)
      (throw (ex-info "invalid party id on edge" {:party party})))
    (when-not (roles role)
      (throw (ex-info "invalid role" {:role role :allowed roles})))
    (when-not (#{:active :revoked} status)
      (throw (ex-info "invalid status" {:status status})))
    (when (and sourcing (not (sourcing-values sourcing)))
      (throw (ex-info "invalid sourcing" {:sourcing sourcing})))
    (cond-> {:party.product/id id
             :party.product/product product
             :party.product/party party
             :party.product/role role
             :party.product/status status
             :party.product/sourcing sourcing}
      unspsc (assoc :party.product/unspsc (digits-only unspsc))
      note (assoc :party.product/note note))))

;; ───────────────────────── graph (pure index over maps) ─────────────────────────

(defn empty-graph
  "Empty in-memory join graph."
  []
  {:parties {}
   :products {}
   :edges {}
   :ledger []})

(defn- index-put [m k v]
  (assoc m k v))

(defn upsert-party
  [g p]
  (let [rec (if (:party/id p)
              (do (when-not (valid-party-id? (:party/id p))
                    (throw (ex-info "invalid party id" {:id (:party/id p)})))
                  p)
              (party p))]
    (update g :parties index-put (:party/id rec) rec)))

(defn upsert-product
  [g p]
  (let [rec (if (:product/id p)
              (do (when-not (valid-product-id? (:product/id p))
                    (throw (ex-info "invalid product id" {:id (:product/id p)})))
                  p)
              (product p))]
    (update g :products index-put (:product/id rec) rec)))

(defn- ensure-edge-map [e]
  (let [em (if (:party.product/id e) e (edge e))]
    (assoc em :party.product/status :active)))

(defn bind
  "Return a new graph with the edge bound (status :active). Upserts the edge;
   appends a ledger fact. Does not auto-create missing product/party entities
   unless `:create-missing?` is true and maps are provided via :product/:party."
  ([g e] (bind g e nil))
  ([g e {:keys [create-missing? product party reason]}]
   (let [em (ensure-edge-map e)
         g (cond-> g
             (and create-missing? product) (upsert-product product)
             (and create-missing? party) (upsert-party party))
         pid (:party.product/product em)
         cid (:party.product/party em)]
     (when-not (get-in g [:products pid])
       (throw (ex-info "product not in graph — upsert product first or pass :create-missing?"
                       {:product pid})))
     (when-not (get-in g [:parties cid])
       (throw (ex-info "party not in graph — upsert party first or pass :create-missing?"
                       {:party cid})))
     (-> g
         (update :edges index-put (:party.product/id em) em)
         (update :ledger conj {:fact :party.product/bind
                               :edge (:party.product/id em)
                               :product pid
                               :party cid
                               :role (:party.product/role em)
                               :reason reason})))))

(defn revoke
  "Return a new graph with the edge status :revoked. Missing edge is an error."
  ([g edge-or-id] (revoke g edge-or-id nil))
  ([g edge-or-id {:keys [reason]}]
   (let [id (if (map? edge-or-id)
              (or (:party.product/id edge-or-id)
                  (edge-id (:party.product/product edge-or-id)
                           (:party.product/role edge-or-id)
                           (:party.product/party edge-or-id)))
              (str edge-or-id))
         existing (get-in g [:edges id])]
     (when-not existing
       (throw (ex-info "edge not found" {:edge id})))
     (-> g
         (assoc-in [:edges id :party.product/status] :revoked)
         (update :ledger conj {:fact :party.product/revoke
                               :edge id
                               :reason reason})))))

(defn active-edges
  [g]
  (->> (vals (:edges g))
       (filter #(= :active (:party.product/status %)))
       vec))

(defn parties-of
  "Active parties linked to a product, optionally filtered by role."
  ([g product-id] (parties-of g product-id nil))
  ([g product-id role]
   (let [pid (str product-id)
         role (some-> role keyword)]
     (->> (active-edges g)
          (filter #(= pid (:party.product/product %)))
          (filter #(or (nil? role) (= role (:party.product/role %))))
          (keep (fn [e]
                  (when-let [p (get-in g [:parties (:party.product/party e)])]
                    (assoc p :party.product/role (:party.product/role e)
                             :party.product/edge (:party.product/id e)))))
          vec))))

(defn products-of
  "Active products linked to a party, optionally filtered by role."
  ([g party-id] (products-of g party-id nil))
  ([g party-id role]
   (let [cid (str party-id)
         role (some-> role keyword)]
     (->> (active-edges g)
          (filter #(= cid (:party.product/party %)))
          (filter #(or (nil? role) (= role (:party.product/role %))))
          (keep (fn [e]
                  (when-let [p (get-in g [:products (:party.product/product e)])]
                    (assoc p :party.product/role (:party.product/role e)
                             :party.product/edge (:party.product/id e)))))
          vec))))

(defn edges-by-role
  [g role]
  (let [role (keyword role)]
    (->> (active-edges g)
         (filter #(= role (:party.product/role %)))
         vec)))

(defn edges-by-unspsc-segment
  "Active edges whose product or edge UNSPSC falls in the given 2-digit segment."
  [g segment]
  (let [seg (unspsc-segment segment)]
    (->> (active-edges g)
         (filter (fn [e]
                   (let [code (or (:party.product/unspsc e)
                                  (get-in g [:products (:party.product/product e)
                                             :product/unspsc]))]
                     (= seg (unspsc-segment code)))))
         vec)))

(defn brand-owner
  "Primary brand-owner party for a product, or nil."
  [g product-id]
  (first (parties-of g product-id :brand-owner)))

;; ───────────────────────── bridges ─────────────────────────

(defn product->unspsc-segment
  "UNSPSC segment for a product record or id looked up in graph."
  ([product-or-code]
   (unspsc-segment
    (cond
      (map? product-or-code) (or (:product/unspsc product-or-code)
                                 (:party.product/unspsc product-or-code)
                                 (:unspsc product-or-code))
      :else product-or-code)))
  ([g product-id]
   (product->unspsc-segment (get-in g [:products (str product-id)]))))

(defn party->itonami-supplier
  "Project a party record into itonami.store supplier shape
   `{:id :name :isic :unspsc-segment :country}`."
  [party]
  (let [p (if (:party/id party) party (party party))]
    (cond-> {:id (:party/id p)
             :name (or (:party/name p) (:party/id p))}
      (:party/isic p) (assoc :isic (:party/isic p))
      (:party/unspsc-segment p) (assoc :unspsc-segment (:party/unspsc-segment p))
      (:party/country p) (assoc :country (:party/country p)))))

(defn itonami-supplier->party
  "Lift an itonami.store supplier map into a party projection.
   Supplier ids that are not already valid party ids are prefixed with `sup-`."
  [{:keys [id name isic unspsc-segment country]}]
  (let [raw (str id)
        pid (if (valid-party-id? raw)
              raw
              (str "sup-" (str/replace raw #"^sup-" "")))]
    (party {:id pid
            :name name
            :kind :supplier
            :isic isic
            :unspsc-segment unspsc-segment
            :country country
            :sourcing :representative})))

(defn product->goyoukiki-unspsc-tags
  "Tags a goyoukiki candidate can carry for a product (segment + commodity)."
  [product]
  (let [code (or (:product/unspsc product) (:unspsc product))
        seg (unspsc-segment code)]
    (cond-> #{}
      seg (conj seg)
      (valid-unspsc-commodity? code) (conj (digits-only code)))))

(defn match-parties-for-product
  "Return active parties that could service a product under procurement-ish
   criteria: same UNSPSC segment (party or edge), optional ISIC match.
   Pure filter — no ranking LLM."
  [g product-id {:keys [role require-isic]}]
  (let [prod (get-in g [:products (str product-id)])
        seg (product->unspsc-segment prod)
        linked (parties-of g product-id role)]
    (->> linked
         (filter (fn [p]
                   (let [pseg (or (:party/unspsc-segment p)
                                  (some-> (get-in g [:edges (:party.product/edge p)
                                                     :party.product/unspsc])
                                          unspsc-segment))]
                     (and (or (nil? seg) (nil? pseg) (= seg pseg))
                          (or (nil? require-isic)
                              (= (str require-isic) (str (:party/isic p))))))))
         vec)))

(defn high-stakes-bind?
  "True when binding this role should be human-gated by an operator governor."
  [role]
  (contains? high-stakes-roles (keyword role)))

(defn validate-bind-request
  "Structural validation for a proposed bind. Returns
   `{:ok? true :edge ... :high-stakes? ...}` or
   `{:ok? false :errors [...]}` — pure, no graph mutation."
  [{:keys [product party role unspsc sourcing] :as req}]
  (let [errs (atom [])
        err! (fn [m] (swap! errs conj m))]
    (try (edge (select-keys req [:product :party :role :unspsc :sourcing :id :note :status]))
         (catch #?(:clj Exception :cljs :default) e
           (err! (ex-message e))))
    (when-not (valid-product-id? (str product)) (err! "invalid product"))
    (when-not (valid-party-id? (str party)) (err! "invalid party"))
    (when-not (roles (keyword role)) (err! "invalid role"))
    (if (seq @errs)
      {:ok? false :errors @errs}
      {:ok? true
       :edge (edge (select-keys req [:product :party :role :unspsc :sourcing :id :note]))
       :high-stakes? (high-stakes-bind? role)})))

;; ───────────────────────── demo seed (uchiwake-aligned) ─────────────────────────

(defn demo-graph
  "Illustrative join graph aligned with uchiwake seed products + kabuto
   company ids. Sourcing is :authoritative for public GTINs / brand-owners
   that are public record; other links :representative. Not exhaustive."
  []
  (let [parties [(party {:id "org.corp.us.coca-cola" :name "The Coca-Cola Company"
                         :kind :company :country "USA" :isic "1104"
                         :sourcing :authoritative})
                 (party {:id "org.corp.it.ferrero" :name "Ferrero"
                         :kind :company :country "ITA" :isic "1073"
                         :sourcing :authoritative})
                 (party {:id "org.corp.us.apple" :name "Apple Inc."
                         :kind :company :country "USA" :isic "2620"
                         :sourcing :authoritative})
                 (party {:id "org.corp.tw.foxconn" :name "Hon Hai Precision (Foxconn)"
                         :kind :company :country "TWN" :isic "2610"
                         :unspsc-segment "32" :sourcing :representative})
                 (party {:id "org.corp.cn.byd" :name "BYD Company"
                         :kind :company :country "CHN" :isic "2910"
                         :unspsc-segment "26" :sourcing :authoritative})
                 (party {:id "sup-aero-blades" :name "Aero Blades K.K."
                         :kind :supplier :country "JPN" :isic "3030"
                         :unspsc-segment "25" :sourcing :representative})
                 (party {:id "merchant.yodobashi_com" :name "Yodobashi Camera"
                         :kind :merchant :country "JPN" :sourcing :representative})]
        products [(product {:gtin "5449000000996"
                            :name "Coca-Cola Classic 330ml can"
                            :brand "Coca-Cola"
                            :unspsc "50202301"
                            :sector :food-beverage
                            :sourcing :authoritative})
                  (product {:gtin "3017620422003"
                            :name "Nutella 750g jar"
                            :brand "Nutella"
                            :unspsc "50161900"
                            :sector :food-beverage
                            :sourcing :authoritative})
                  (product {:id "prod.smartphone-flagship"
                            :name "Flagship smartphone (representative teardown)"
                            :brand "(representative)"
                            :unspsc "43191501"
                            :sector :electronics
                            :sourcing :representative})
                  (product {:id "prod.ev-battery-pack"
                            :name "EV traction battery pack (NMC, representative)"
                            :unspsc "26111701"
                            :sector :automotive
                            :sourcing :representative})
                  (product {:id "prod.engine-blade-set"
                            :name "Turbofan compressor blade set (demo)"
                            :unspsc "25101504"
                            :sector :aerospace
                            :sourcing :synthesized})]
        edges [(edge {:product "gtin.05449000000996" :party "org.corp.us.coca-cola"
                      :role :brand-owner :unspsc "50202301" :sourcing :authoritative})
               (edge {:product "gtin.03017620422003" :party "org.corp.it.ferrero"
                      :role :brand-owner :unspsc "50161900" :sourcing :authoritative})
               (edge {:product "prod.smartphone-flagship" :party "org.corp.us.apple"
                      :role :brand-owner :unspsc "43191501" :sourcing :representative})
               (edge {:product "prod.smartphone-flagship" :party "org.corp.tw.foxconn"
                      :role :assembler :unspsc "43191501" :sourcing :representative})
               (edge {:product "prod.ev-battery-pack" :party "org.corp.cn.byd"
                      :role :brand-owner :unspsc "26111701" :sourcing :representative})
               (edge {:product "prod.engine-blade-set" :party "sup-aero-blades"
                      :role :supplier :unspsc "25101504" :sourcing :synthesized})
               (edge {:product "gtin.05449000000996" :party "merchant.yodobashi_com"
                      :role :merchant :unspsc "50202301" :sourcing :representative})]]
    (reduce (fn [g e]
              (bind g e))
            (reduce upsert-product
                    (reduce upsert-party (empty-graph) parties)
                    products)
            edges)))

(defn graph-summary
  "Counts for operator consoles / doctor checks."
  [g]
  (let [ae (active-edges g)]
    {:parties (count (:parties g))
     :products (count (:products g))
     :edges (count (:edges g))
     :active-edges (count ae)
     :revoked-edges (count (filter #(= :revoked (:party.product/status %))
                                   (vals (:edges g))))
     :by-role (frequencies (map :party.product/role ae))
     :ledger (count (:ledger g))}))
