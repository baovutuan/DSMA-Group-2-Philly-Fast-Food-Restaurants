SELECT 
bus.j ->> 'business_id' AS business_id,
bus.j ->> 'name' AS business_name,
bus.j ->> 'latitude' AS business_lat,
bus.j ->> 'longitude' AS business_long,
bus.j->'attributes'->>'RestaurantsPriceRange2' AS business_price,
(CASE WHEN STRPOS(bus.j->'attributes'->> 'BusinessParking', 'True') <> 0 THEN 'true'
 ELSE 'false' END) AS business_park,
(CASE
        WHEN bus.j->'attributes'->>'WiFi' IN ('u''no''', '''no''', 'None') THEN 'No'
        WHEN bus.j->'attributes'->>'WiFi' IN ('u''free''', '''free''') THEN 'Free'
        WHEN bus.j->'attributes'->>'WiFi' IN ('u''paid''', '''paid''') THEN 'Paid'
        ELSE NULL
    END) AS business_wifi, -- Modifying WiFi data like this: https://www.kaggle.com/code/wenqihou828/recommendation-for-yelp-users-itself
bus.j->'attributes'->> 'RestaurantsTakeOut' AS business_takeout,
bus.j->'attributes'->> 'BusinessAcceptsCreditCards' AS business_creditcards,
bus.j->'attributes'->> 'RestaurantsDelivery' AS business_delivery
FROM
public.business AS bus
WHERE bus.j ->> 'city' = 'Philadelphia'
AND (bus.j->>'is_open')::int = 1
AND bus.j->>'categories' LIKE '%Fast Food%' 
;