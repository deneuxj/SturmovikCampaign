﻿#r "nuget: FSharp.Charting"
#r "System.Windows.Forms.DataVisualization"

open FSharp.Charting

[[(107.13, 62.920002); (107.78, 65.17); (106.26, 68.58); (103.82, 69.36);
(99.2, 64.97); (99.35, 64.92)];
      [(10.82, 29.55); (11.88, 36.71); (8.040001, 37.989998); (4.2, 38.66);
(3.1399999, 31.5); (4.65, 30.509998)];
      [(45.31, 69.42); (49.15, 72.1); (50.21, 75.909996); (42.98, 77.479996)];
      [(53.64, 19.599998); (57.88, 19.849998); (53.64, 27.619999)];
      [(64.42, 88.89); (74.03999, 95.03); (71.729996, 97.659996); (66.42, 98.03)];
      [(28.51, 99.24); (32.35, 100.2); (35.13, 101.77); (36.190002, 107.299995);
(27.45, 103.72)];
      [(7.86, 49.52); (15.09, 52.370003); (14.030001, 56.850002); (10.150001, 54.82)];
      [(8.27, 82.93); (11.14, 84.32); (13.38, 86.45); (4.4300003, 90.259995)];
      [(31.29, 84.46); (37.46, 85.74); (38.52, 87.31); (38.97, 92.52);
(35.13, 93.799995); (32.35, 88.27)];
      [(88.35, 80.399994); (91.13, 84.21); (82.18, 85.78); (84.509995, 81.07)];
      [(48.31, 81.06); (49.95, 86.94); (49.49, 87.9); (46.33, 87.91)];
      [(33.29, 100.36); (38.86, 101.799995); (40.77, 102.63); (40.29, 105.03)];
      [(30.850002, 19.24); (29.79, 27.68); (25.95, 25.0); (23.62, 23.05);
(22.11, 19.619999)];
      [(19.76, 64.98); (22.0, 65.020004); (23.51, 72.41); (19.67, 73.08);
(15.83, 72.12); (16.89, 69.270004)];
      [(66.87, 99.85); (67.93, 105.38); (64.54, 107.909996); (63.03, 108.899994);
(60.7, 106.95); (60.79, 100.77)];
      [(20.68, 14.58); (21.740002, 18.39); (16.28, 23.61); (14.51, 23.92);
(15.57, 15.48); (19.619999, 14.63)];
      [(28.5, 47.949997); (28.95, 53.16); (26.17, 56.01); (20.0, 56.969997);
(20.21, 55.78); (22.33, 48.91)];
      [(19.75, 67.259995); (18.69, 71.74); (14.85, 73.02); (13.58, 72.64);
(12.52, 71.07); (15.91, 67.93)]
]
|> List.map (fun poly -> List.last poly :: poly)
|> List.map (fun poly -> Chart.Line(poly, Color=System.Drawing.Color.Blue))
|> List.append [Chart.Line([(74.299995, 49.76); (83.25, 48.19); (77.079994, 56.92); (74.299995, 49.76)], Color=System.Drawing.Color.Red)]
|> Chart.Combine
|> fun x -> x.ShowChart()