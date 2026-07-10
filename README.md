# kotoba-product-party

Portable **product ↔ party (company / supplier / merchant)** join graph for
kotoba-lang and cloud-itonami open businesses.

This is the missing operator-facing edge between:

| Concern | SSoT (not this lib) |
|---|---|
| Trade-item identity (GTIN) | etzhayyim GTIN actor / uchiwake |
| Listed companies (`org.corp.*`) | etzhayyim kabuto |
| Commodity codes (8-digit UNSPSC) | etzhayyim `20-actors/unspsc` |
| Segment open-business blueprints | `kotoba-lang/unspsc` |
| Industry open-business blueprints | `kotoba-lang/industry` |

`product-party` owns only the **join**: which party plays which role on which
product, tagged with UNSPSC / ISIC so procurement and open-business routing
can resolve without re-deriving identity graphs.

## Contract

```clojure
(require '[kotoba.product-party :as pp])

(def g (pp/demo-graph))

(pp/brand-owner g "gtin.05449000000996")
;; => party map for org.corp.us.coca-cola

(pp/parties-of g "prod.smartphone-flagship")
;; => brand-owner Apple + assembler Foxconn

(pp/bind g (pp/edge {:product "prod.engine-blade-set"
                     :party "sup-aero-blades"
                     :role :supplier
                     :unspsc "25101504"}))

(pp/party->itonami-supplier
  (pp/party {:id "sup-aero-blades" :kind :supplier :isic "3030"
             :unspsc-segment "25" :country "JPN"}))

;; Bulk import uchiwake / product-bom entity maps
(pp/import-entities (pp/empty-graph) entities)
```

Workspace effects in `gftdcojp/cloud-itonami`:

| Kind | Handler |
|---|---|
| `:product-party/bind` | interactive bind (high-stakes gated) |
| `:product-party/revoke` | interactive revoke (high-stakes gated) |
| `:product-party/import-entities` | bulk import (`:entities` / JVM `:path` / `:seed`) |

### Roles

`:brand-owner` · `:manufacturer` · `:assembler` · `:supplier` ·
`:distributor` · `:merchant` · `:carrier` · `:operator`

`:brand-owner` and `:manufacturer` are **high-stakes** (operator governors
should human-gate binds/revokes).

### Id spaces

| Kind | Pattern | Example |
|---|---|---|
| Product (GTIN) | `gtin.<14>` | `gtin.05449000000996` |
| Product (slug) | `prod.<slug>` | `prod.smartphone-flagship` |
| Company (kabuto) | `org.corp.<cc>.<slug>` | `org.corp.us.apple` |
| Supplier | `sup-<id>` | `sup-aero-blades` |
| Merchant | `merchant.<apex>` | `merchant.yodobashi_com` |

## cloud-itonami

See [`docs/cloud-itonami.md`](docs/cloud-itonami.md). The runtime facade is
`cloud-itonami.product-party` in `gftdcojp/cloud-itonami`.

## Test

```bash
clojure -M:test
```

## License

Apache-2.0
