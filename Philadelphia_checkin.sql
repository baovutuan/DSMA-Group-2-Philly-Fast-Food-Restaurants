SELECT DISTINCT
	chn.j->>'business_id' AS business_id, 
	DATE(unnest(string_to_array(chn.j->>'date', ','))) AS chn_date
FROM public.checkin AS chn, public.business AS bus
WHERE chn.j->>'business_id' = bus.j->>'business_id' 
AND bus.j ->> 'city' = 'Philadelphia'
AND (bus.j->>'is_open')::int = 1
AND bus.j->>'categories' LIKE '%Fast Food%'; 