(ns kotoba.product-party-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.product-party :as pp]))

(deftest gtin-normalize-and-check
  (testing "EAN-13 pads to GTIN-14 and validates check digit"
    (is (= "05449000000996" (pp/normalize-gtin "5449000000996")))
    (is (pp/valid-gtin? "5449000000996"))
    (is (pp/valid-gtin? "05449000000996"))
    (is (pp/valid-gtin? "3017620422003")))
  (testing "bad check digit rejected"
    (is (not (pp/valid-gtin? "5449000000990"))))
  (testing "product id from gtin"
    (is (= "gtin.05449000000996" (pp/product-id "5449000000996")))
    (is (pp/valid-product-id? "gtin.05449000000996"))
    (is (pp/valid-product-id? "prod.smartphone-flagship"))
    (is (not (pp/valid-product-id? "nope")))))

(deftest party-and-edge-constructors
  (let [p (pp/party {:id "org.corp.us.apple" :name "Apple" :isic "2620" :country "USA"})
        prod (pp/product {:gtin "5449000000996" :name "Coke" :unspsc "50202301"})
        e (pp/edge {:product (:product/id prod)
                    :party (:party/id p)
                    :role :brand-owner
                    :unspsc "50202301"})]
    (is (= :company (:party/kind p)))
    (is (= "05449000000996" (:product/gtin prod)))
    (is (= :brand-owner (:party.product/role e)))
    (is (thrown? Exception (pp/party {:id "not-a-party"})))
    (is (thrown? Exception (pp/edge {:product "gtin.05449000000996"
                                     :party "org.corp.us.apple"
                                     :role :not-a-role})))))

(deftest graph-bind-query-revoke
  (let [g0 (pp/demo-graph)
        coke "gtin.05449000000996"]
    (is (= "org.corp.us.coca-cola" (:party/id (pp/brand-owner g0 coke))))
    (is (some #{:merchant} (map :party.product/role (pp/parties-of g0 coke))))
    (is (seq (pp/products-of g0 "org.corp.us.apple" :brand-owner)))
    (is (seq (pp/edges-by-unspsc-segment g0 "50")))
    (is (= "25" (pp/product->unspsc-segment g0 "prod.engine-blade-set")))
    (let [edge-id (:party.product/id
                   (first (filter #(= :merchant (:party.product/role %))
                                  (pp/active-edges g0))))
          g1 (pp/revoke g0 edge-id {:reason "delisted"})]
      (is (= :revoked (get-in g1 [:edges edge-id :party.product/status])))
      (is (empty? (pp/parties-of g1 coke :merchant)))
      (is (pos? (:ledger (pp/graph-summary g1)))))))

(deftest bridges
  (let [sup (pp/party->itonami-supplier
             (pp/party {:id "sup-aero-blades" :name "Aero Blades K.K."
                        :kind :supplier :isic "3030" :unspsc-segment "25"
                        :country "JPN"}))
        lifted (pp/itonami-supplier->party
                {:id "sup-1" :name "Aero" :isic "3030"
                 :unspsc-segment "25" :country "JPN"})
        tags (pp/product->goyoukiki-unspsc-tags
              {:product/unspsc "25101504"})]
    (is (= "3030" (:isic sup)))
    (is (= "25" (:unspsc-segment sup)))
    (is (pp/valid-party-id? (:party/id lifted)))
    (is (contains? tags "25"))
    (is (contains? tags "25101504"))))

(deftest validate-bind-and-match
  (let [g (pp/demo-graph)
        ok (pp/validate-bind-request
            {:product "gtin.05449000000996"
             :party "org.corp.us.coca-cola"
             :role :brand-owner})
        bad (pp/validate-bind-request
             {:product "nope" :party "also-nope" :role :brand-owner})
        matched (pp/match-parties-for-product
                 g "prod.engine-blade-set" {:role :supplier :require-isic "3030"})]
    (is (:ok? ok))
    (is (:high-stakes? ok))
    (is (not (:ok? bad)))
    (is (= ["sup-aero-blades"] (mapv :party/id matched)))))

(deftest import-uchiwake-shaped-entities
  (let [entities
        [{:product/id "gtin.05449000000996" :product/gtin "05449000000996"
          :product/name "Coca-Cola Classic 330ml can" :product/brand "Coca-Cola"
          :product/brand-owner "org.corp.us.coca-cola"
          :product/unspsc "50202301" :product/sourcing :authoritative}
         {:product/id "prod.smartphone-flagship"
          :product/name "Flagship smartphone" :product/brand-owner "org.corp.us.apple"
          :product/unspsc "43191501" :product/sourcing :representative}
         {:bom.edge/id "bom.phone.soc"
          :bom.edge/parent "prod.smartphone-flagship" :bom.edge/child "part.soc"
          :bom.edge/supplier "org.corp.tw.tsmc" :bom.edge/sourcing :representative}
         ;; part-parent supplier must NOT become a product edge
         {:bom.edge/id "bom.cell.co"
          :bom.edge/parent "part.li-ion-cell" :bom.edge/child "mat.cobalt"
          :bom.edge/supplier "org.corp.ch.glencore" :bom.edge/sourcing :representative}
         {:process.step/id "proc.phone-asm" :process.step/kind :assembly
          :process.step/of "prod.smartphone-flagship"
          :process.step/operator "org.corp.tw.foxconn"
          :process.step/sourcing :representative}
         {:logistics.leg/id "leg.tshirt"
          :logistics.leg/of "prod.smartphone-flagship"
          :logistics.leg/carrier "org.corp.dk.maersk"
          :logistics.leg/sourcing :representative}]
        before (pp/empty-graph)
        after (pp/import-entities before entities)
        report (pp/import-report before after)]
    (is (= "org.corp.us.coca-cola"
           (:party/id (pp/brand-owner after "gtin.05449000000996"))))
    (is (some #{"org.corp.tw.tsmc"}
              (map :party/id (pp/parties-of after "prod.smartphone-flagship" :supplier))))
    (is (some #{"org.corp.tw.foxconn"}
              (map :party/id (pp/parties-of after "prod.smartphone-flagship" :assembler))))
    (is (some #{"org.corp.dk.maersk"}
              (map :party/id (pp/parties-of after "prod.smartphone-flagship" :carrier))))
    (is (nil? (get-in after [:products "part.li-ion-cell"])))
    (is (pos? (get-in report [:added :products])))
    (is (pos? (get-in report [:added :edges])))))
