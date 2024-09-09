xray_l5_segment_angle_function <- function(pelvic_incidence = 51.66,l5_pelvic_angle = 5.295,l4_pelvic_angle = 8.275) {-2.0202463+0.86132432*pelvic_incidence+1.5205363*l5_pelvic_angle-3.0319644*l4_pelvic_angle }
  
xray_l4_segment_angle_function <- function(pelvic_incidence = 51.66,l5_pelvic_angle = 5.295,l4_pelvic_angle = 8.275,l3_pelvic_angle = 7.605,l5_s1 = 24.885) {-1.838579+0.6819925*pelvic_incidence+0.13215596*l5_pelvic_angle+2.5597819*l4_pelvic_angle-3.3240951*l3_pelvic_angle-0.68003513*l5_s1 }

xray_l3_segment_angle_function <- function(pelvic_incidence = 51.66,l4_pelvic_angle = 8.275,l3_pelvic_angle = 7.605,l2_pelvic_angle = 5.735,l4_s1 = 37.77) {0.95842314+0.61524052*pelvic_incidence+0.82076607*l4_pelvic_angle+1.9164608*l3_pelvic_angle-3.2512557*l2_pelvic_angle-0.64281668*l4_s1 }

xray_l2_segment_angle_function <- function(pelvic_incidence = 51.66,l3_pelvic_angle = 7.605,l2_pelvic_angle = 5.735,l1_pelvic_angle = 3.86,l3_s1 = 49.12) {-0.85599789+0.46255635*pelvic_incidence+0.7923093*l3_pelvic_angle+2.1727065*l2_pelvic_angle-3.3450882*l1_pelvic_angle-0.45546378*l3_s1 }

xray_l1_segment_angle_function <- function(pelvic_incidence = 51.66,l2_pelvic_angle = 5.735,l1_pelvic_angle = 3.86,t12_pelvic_angle = 2.185,l2_s1 = 55.36) {0.76072-0.36785867*pelvic_incidence+1.0257968*l2_pelvic_angle+1.2947341*l1_pelvic_angle-1.5716106*t12_pelvic_angle+0.31545016*l2_s1 }

xray_t12_segment_angle_function <- function(pelvic_incidence = 51.66,l1_pelvic_angle = 3.86,t12_pelvic_angle = 2.185,t11_pelvic_angle = 1.385,l1_s1 = 58.895) {0.84526326+0.86614553*pelvic_incidence+3.2493695*l1_pelvic_angle+1.2659366*t12_pelvic_angle-5.3943619*t11_pelvic_angle-0.86689216*l1_s1 }

xray_t11_segment_angle_function <- function(pelvic_incidence = 51.66,t12_pelvic_angle = 2.185,t11_pelvic_angle = 1.385,t10_pelvic_angle = 1.08,t12_s1 = 61.48815) {0.72246115+0.85133542*pelvic_incidence+3.0756192*t12_pelvic_angle+3.405949*t11_pelvic_angle-7.3291311*t10_pelvic_angle-0.85420293*t12_s1 }

xray_t10_segment_angle_function <- function(pelvic_incidence = 51.66,t11_pelvic_angle = 1.385,t10_pelvic_angle = 1.08,t9_pelvic_angle = 0.8,t11_s1 = 59.33755) {0.97721293+0.82420744*pelvic_incidence+3.5366711*t11_pelvic_angle+3.6827362*t10_pelvic_angle-8.0393362*t9_pelvic_angle-0.83731058*t11_s1 }

xray_t9_segment_angle_function <- function(pelvic_incidence = 51.66,t10_pelvic_angle = 1.08,t9_pelvic_angle = 0.8,t8_pelvic_angle = 0.715,t10_s1 = 55.06625) {0.30595552+0.81766194*pelvic_incidence+3.8780922*t10_pelvic_angle+4.4407352*t9_pelvic_angle-9.1232178*t8_pelvic_angle-0.81302897*t10_s1 }

xray_t8_segment_angle_function <- function(pelvic_incidence = 51.66,t9_pelvic_angle = 0.8,t8_pelvic_angle = 0.715,t7_pelvic_angle = 0.995,t9_s1 = 52.6097) {0.91157466+0.87324599*pelvic_incidence+6.1758776*t9_pelvic_angle+1.9019557*t8_pelvic_angle-8.9677854*t7_pelvic_angle-0.88142083*t9_s1 }

xray_t7_segment_angle_function <- function(pelvic_incidence = 51.66,t8_pelvic_angle = 0.715,t7_pelvic_angle = 0.995,t6_pelvic_angle = 1.505,t8_s1 = 49.2667) {0.42533226+0.91111859*pelvic_incidence+6.4355557*t8_pelvic_angle+3.4236469*t7_pelvic_angle-10.768299*t6_pelvic_angle-0.90935568*t8_s1 }

xray_t6_segment_angle_function <- function(pelvic_incidence = 51.66,t7_pelvic_angle = 0.995,t6_pelvic_angle = 1.505,t5_pelvic_angle = 2.295,t7_s1 = 43.3736) {-0.34545776+0.87999133*pelvic_incidence+6.3698086*t7_pelvic_angle+4.3968958*t6_pelvic_angle-11.642594*t5_pelvic_angle-0.86715392*t7_s1 }

xray_t5_segment_angle_function <- function(pelvic_incidence = 51.66,t6_pelvic_angle = 1.505,t5_pelvic_angle = 2.295,t4_pelvic_angle = 3.3,t6_s1 = 37.78415) {-0.17723591+0.89935671*pelvic_incidence+9.4645673*t6_pelvic_angle+0.40522763*t5_pelvic_angle-10.777907*t4_pelvic_angle-0.89084277*t6_s1 }

xray_t4_segment_angle_function <- function(pelvic_incidence = 51.66,t5_pelvic_angle = 2.295,t4_pelvic_angle = 3.3,t3_pelvic_angle = 4.26,t5_s1 = 31.6964) {-0.38375283+0.93048698*pelvic_incidence+9.6638335*t5_pelvic_angle+2.171303*t4_pelvic_angle-12.758863*t3_pelvic_angle-0.90732488*t5_s1 }

xray_t3_segment_angle_function <- function(pelvic_incidence = 51.66,t4_pelvic_angle = 3.3,t3_pelvic_angle = 4.26,t2_pelvic_angle = 5.325,t4_s1 = 26.6511) {-0.95497132+0.78298321*pelvic_incidence+7.9309047*t4_pelvic_angle+3.6473992*t3_pelvic_angle-12.339865*t2_pelvic_angle-0.76274627*t4_s1 }

xray_t2_segment_angle_function <- function(pelvic_incidence = 51.66,t3_pelvic_angle = 4.26,t2_pelvic_angle = 5.325,t1_pelvic_angle = 6.225,t3_s1 = 21.4441) {-1.0347681+0.73996468*pelvic_incidence+10.652171*t3_pelvic_angle-2.4960373*t2_pelvic_angle-8.8582603*t1_pelvic_angle-0.72385763*t3_s1 }

xray_t1_segment_angle_function <- function(pelvic_incidence = 51.66,t2_pelvic_angle = 5.325,t1_pelvic_angle = 6.225,c7_pelvic_angle = 6.985,t2_s1 = 19.25295) {-1.9734033+0.9465077*pelvic_incidence+11.281071*t2_pelvic_angle+2.3514443*t1_pelvic_angle-14.565216*c7_pelvic_angle-0.91947645*t2_s1 }

xray_c7_segment_angle_function <- function(pelvic_incidence = 51.66,t1_pelvic_angle = 6.225,c7_pelvic_angle = 6.985,c6_pelvic_angle = 7.51,t1_s1 = 20.56835) {1.8838877+0.87256661*pelvic_incidence+8.4121312*t1_pelvic_angle+9.852786*c7_pelvic_angle-19.13112*c6_pelvic_angle-0.82726714*t1_s1 }

xray_c6_segment_angle_function <- function(pelvic_incidence = 51.66,c7_pelvic_angle = 6.985,c6_pelvic_angle = 7.51,c5_pelvic_angle = 7.835,c7_s1 = 29.1385) {4.3109454+0.83290109*pelvic_incidence+12.778497*c7_pelvic_angle+2.8888087*c6_pelvic_angle-16.522542*c5_pelvic_angle-0.84626573*c7_s1 }

xray_c5_segment_angle_function <- function(pelvic_incidence = 51.66,c6_pelvic_angle = 7.51,c5_pelvic_angle = 7.835,c4_pelvic_angle = 8.335,c6_s1 = 34.61925) {1.6074564+0.81348424*pelvic_incidence+12.14449*c6_pelvic_angle+4.3448417*c5_pelvic_angle-17.307435*c4_pelvic_angle-0.80999033*c6_s1 }

xray_c4_segment_angle_function <- function(pelvic_incidence = 51.66,c5_pelvic_angle = 7.835,c4_pelvic_angle = 8.335,c3_pelvic_angle = 8.81,c5_s1 = 33.03205) {-2.6059537+0.79956719*pelvic_incidence+9.3589451*c5_pelvic_angle+8.6166598*c4_pelvic_angle-18.75846*c3_pelvic_angle-0.76585164*c5_s1 }

xray_c3_segment_angle_function <- function(pelvic_incidence = 51.66,c4_pelvic_angle = 8.335,c3_pelvic_angle = 8.81,c2_pelvic_angle = 8.965,c4_s1 = 29.9114) {-0.084900811+0.79976049*pelvic_incidence+14.81046*c4_pelvic_angle-3.679594*c3_pelvic_angle-11.911836*c2_pelvic_angle-0.80666906*c4_s1 }

xray_c2_segment_angle_function <- function(pelvic_incidence = 51.66,c3_pelvic_angle = 8.81,c2_pelvic_angle = 8.965,c3_s1 = 32.63065) {3.957873+0.91370275*pelvic_incidence+27.475915*c3_pelvic_angle-28.344052*c2_pelvic_angle-0.91780213*c3_s1 }