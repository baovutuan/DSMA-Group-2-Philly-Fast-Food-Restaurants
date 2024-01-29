SELECT 
rev.j ->> 'review_id' AS review_id,
rev.j ->> 'date' AS date,
rev.j ->> 'business_id' AS business_id,
rev.j ->> 'text' AS review,
rev.j ->> 'user_id' AS user_id,
rev.j ->> 'stars' AS stars,
usr.j ->> 'review_count' AS user_review_count,
usr.j ->> 'elite' AS elite_user,
usr.j ->> 'friends' AS friends,
usr.j ->> 'name' AS name,
usr.j ->> 'fans' AS fans
FROM
public.review AS rev
INNER JOIN
public.business AS bus ON rev.j ->> 'business_id' = bus.j ->> 'business_id'
INNER JOIN
public.users AS usr ON rev.j ->> 'user_id' = usr.j ->> 'user_id'
WHERE rev.j ->> 'business_id' = bus.j ->> 'business_id' 
AND bus.j ->> 'city' = 'Philadelphia'
AND (bus.j->>'is_open')::int = 1
AND bus.j->>'categories' LIKE '%Fast Food%' 
;