WITH STORE_STATE AS (
	SELECT DISTINCT
		WM_TIENDA AS STORE_NBR,
		WM_EDO AS STATE_NAME,
		CASE 
			WHEN WM_EDO = 'Baja California Sur' THEN 'BCS'
			ELSE EDO3D
		END AS STATE_CODE
	FROM
		WM_AD_HOC.T_TDAS
),
STATE_STORES AS (
	SELECT 
		CASE 
			WHEN WM_EDO = 'Baja California Sur' THEN 'BCS'
			ELSE EDO3D
		END AS STATE_CODE,
		COUNT(DISTINCT(WM_TIENDA)) AS N_TIENDAS
	FROM
		WM_AD_HOC.T_TDAS
	GROUP BY
		1
),
PRE_QUERY AS (
	SELECT
		(NS.SOURCE_ID - 70000) AS DC,
		NS.STORE_NBR,
		ST.STATE_NAME,
		ST.STATE_CODE,
		--CAST((IT.VENDOR_NBR * 1000) + (IT.VENDOR_DEPT_NBR * 10) + (IT.VENDOR_SEQ_NBR) AS INTEGER) AS VENDOR9,
		--(EXTRACT (YEAR FROM CAL.GREGORIAN_DATE) * 100) + EXTRACT (MONTH FROM CAL.GREGORIAN_DATE) AS YEAR_MONTH,
		--CAL.GREGORIAN_DATE,
		CAL.WM_YR_WK,
		CAST(
			(
				NS.SAT_SHIP_QTY * CAL.SAT_MULT +
				NS.SUN_SHIP_QTY * CAL.SUN_MULT +
				NS.MON_SHIP_QTY * CAL.MON_MULT +
				NS.TUE_SHIP_QTY * CAL.TUE_MULT +
				NS.WED_SHIP_QTY * CAL.WED_MULT +
				NS.THU_SHIP_QTY * CAL.THU_MULT +
				NS.FRI_SHIP_QTY * CAL.FRI_MULT
			) / IT.VNPK_QTY
			AS NUMERIC(15,4)
		) AS CASES,
		CAST(
			CASE 
				WHEN (IT.PALLET_TI_QTY * IT.PALLET_HI_QTY) = 0 THEN 0
				ELSE CASES / (IT.PALLET_TI_QTY * IT.PALLET_HI_QTY)
			END
		AS NUMERIC(15,4)) AS PALLETS
	FROM 
		MX_CF_VM.SKU_DLY_SHIP AS NS
		INNER JOIN MX_CF_VM.ITEM IT
		ON NS.ITEM_NBR = IT.ITEM_NBR
		INNER JOIN MX_CF_VM.CALENDAR_DAY CAL
		ON NS.WM_YR_WK = CAL.WM_YR_WK
		INNER JOIN STORE_STATE ST
		ON NS.STORE_NBR = ST.STORE_NBR
	WHERE
		CASES > 0
		--AND YEAR_MONTH = 202002
		AND NS.WM_YR_WK BETWEEN 12022 AND 12026
		-- QUITA TODO LO DE PERECEDEROS
		--AND DC NOT IN (7450, 7499, 7495, 7472, 7466, 7498, 7454, 7491)
),
WEEK_SUMMARY AS (
	SELECT 
		STATE_NAME,
		STATE_CODE,
		WM_YR_WK,
		SUM(CASES) AS CASES,
		SUM(PALLETS) AS PALLETS
	FROM 
		PRE_QUERY
	GROUP BY 
		1, 2, 3
)
SELECT 
	WS.STATE_NAME,
	WS.STATE_CODE,
	SST.N_TIENDAS,
	AVERAGE(WS.CASES) AS CASES,
	AVERAGE(WS.PALLETS) AS PALLETS,
	COUNT(WS.WM_YR_WK) AS N_SEMANAS
FROM 
	WEEK_SUMMARY WS
	LEFT JOIN STATE_STORES SST
	ON SST.STATE_CODE = WS.STATE_CODE
GROUP BY 
	1, 2, 3
ORDER BY 1, 2
