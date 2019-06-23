(def Oz (make-variable '(3.4482172403827303 3.3019272488946263 2.2894284851066637 2.6207413942088964 2.8438669798515654 2.668401648721945 2.0 2.5198420997897464 2.2239800905693152 2.4101422641752297 2.6207413942088964 2.4101422641752297 3.239611801277483 1.8171205928321397 3.1072325059538586 2.2239800905693152 1.0 2.2239800905693152 1.5874010519681994 3.1748021039363987 2.8438669798515654 3.5568933044900626 4.862944131094279 3.332221851645953 3.072316825685847 4.140817749422853 3.3912114430141664 2.8438669798515654 2.7589241763811203 3.332221851645953 2.7144176165949063 2.2894284851066637 2.3513346877207573 5.12992784003009 3.6593057100229713 3.1748021039363987 3.9999999999999996 3.4199518933533937 4.254320865115005 4.594700892207039 4.594700892207039 4.396829672158179 2.154434690031884 3.0 1.912931182772389 3.634241185664279 3.2710663101885897 3.936497183102173 4.290840427026207 3.9790572078963917 2.5198420997897464 4.308869380063767 4.762203155904598 2.7144176165949063 3.7325111568172478 4.344481485768611 3.6840314986403864 3.9999999999999996 3.8929964158732604 3.3912114430141664 2.080083823051904 2.5198420997897464 4.9596756638423 4.464745095584537 4.791419857062784 3.530348335326063 3.0365889718756622 4.020725758589058 2.802039330655387 3.8929964158732604 2.8438669798515654 3.1413806523913927 3.530348335326063 2.7589241763811203 2.080083823051904 3.5568933044900626 5.517848352762241 4.179339196381232 4.235823584254893 4.904868131524016 4.379519139887889 4.396829672158179 4.5788569702133275 4.272658681697917 4.179339196381232 4.497941445275415 3.6088260801386944 3.1748021039363987 2.7144176165949063 2.8438669798515654 2.7589241763811203 2.8844991406148166 3.530348335326063 2.7589241763811203 3.0365889718756622 2.080083823051904 2.3513346877207573 3.583047871015946 2.6207413942088964 2.3513346877207573 2.8844991406148166 2.5198420997897464 2.3513346877207573 2.8438669798515654 3.3019272488946263 1.912931182772389 2.4101422641752297 3.1072325059538586 2.4101422641752297 2.6207413942088964 2.7144176165949063) :name 'Oz :role 'Y))

(def SolR (make-variable '(190 118 149 313 299 99 19 256 290 274 65 334 307 78 322 44 8 320 25 92 13 252 223 279 127 291 323 148 191 284 37 120 137 269 248 236 175 314 276 267 272 175 264 175 48 260 274 285 187 220 7 294 223 81 82 213 275 253 254 83 24 77 255 229 207 192 273 157 71 51 115 244 190 259 36 212 238 215 203 225 237 188 167 197 183 189 95 92 252 220 230 259 236 259 238 24 112 237 224 27 238 201 238 14 139 49 20 193 191 131 223) :name 'SolR :role 'X))

(def Wind (make-variable '(7.4 8 12.6 11.5 8.6 13.8 20.1 9.699999999999999 9.199999999999999 10.9 13.2 11.5 12 18.4 11.5 9.699999999999999 9.699999999999999 16.6 9.699999999999999 12 12 14.9 5.7 7.4 9.699999999999999 13.8 11.5 8 14.9 20.7 9.199999999999999 11.5 10.3 4 9.199999999999999 9.199999999999999 4.6 10.9 5.1 6.3 5.7 7.4 14.3 14.9 14.3 6.9 10.3 6.3 5.1 11.5 6.9 8.6 8 8.6 12 7.4 7.4 7.4 9.199999999999999 6.9 13.8 7.4 4 10.3 8 11.5 11.5 9.699999999999999 10.3 6.3 7.4 10.9 10.3 15.5 14.3 9.699999999999999 3.4 8 9.699999999999999 2.3 6.3 6.3 6.9 5.1 2.8 4.6 7.4 15.5 10.9 10.3 10.9 9.699999999999999 14.9 15.5 6.3 10.9 11.5 6.9 13.8 10.3 10.3 8 12.6 9.199999999999999 10.3 10.3 16.6 6.9 14.3 8 11.5) :name 'Wind :role 'Z))

(def Temp (make-variable '(67 72 74 62 65 59 61 69 66 68 58 64 66 57 68 62 59 73 61 61 67 81 79 76 82 90 87 82 77 72 65 73 76 84 85 81 83 83 88 92 92 89 73 81 80 81 82 84 87 85 74 86 85 82 86 88 86 83 81 81 81 82 89 90 90 86 82 80 77 79 76 78 78 77 72 79 81 86 97 94 96 94 91 92 93 93 87 84 80 78 75 73 81 76 77 71 71 78 67 76 68 82 64 71 81 69 63 70 75 76 68) :name 'Temp :role 'Z))

(def Air (make-dataset (list Oz SolR Wind Temp) :dataset-name 'Air))