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
