(def TYP (make-variable '(FH-1 FJ-1 F-86A F9F-2 F-94A F3D-1 F-89A XF10F-1 F9F-6 F-100A F4D-1 F11F-1 F-101A F3H-2 F-102A F-8A F-104B F-105B YF-107A F-106A F-4B F-111A) :name 'TYP))

(def SPR (make-variable '(1.468 1.605 2.168 2.054 2.467 1.294 2.183 2.426 2.607 4.567 4.588 3.618 5.855 2.898 3.88 0.455 8.087999999999999 6.502 6.081 7.105 8.548 6.321) :name 'SPR))

(def CAR (make-variable '(0 0 1 0 1 0 1 0 0 1 0 0 1 0 1 0 1 1 1 1 0 1)
	:name 'CAR :type 'C))

(def Aircraft (make-dataset (list TYP SPR CAR) :dataset-name 'Aircraft))