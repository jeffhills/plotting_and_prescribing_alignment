thoracic_segment_angle_function <- function(t_level = "t12", pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634){
  if(t_level == "t12"){segment_angle <- 1.6001133-0.095010793*pelvic_incidence-0.1674236*pt+0.49299555*l1_pelvic_angle+0.04476664*l1_s1}
  if(t_level == "t11"){segment_angle <- 3.5640583+0.098994722*pelvic_incidence-0.3102797*pt+0.31710505*l1_pelvic_angle-0.14525513*l1_s1}
  if(t_level == "t10"){segment_angle <- 2.9889097+0.25080261*pelvic_incidence-0.32976469*pt+0.050440119*l1_pelvic_angle-0.2552071*l1_s1}
  if(t_level == "t9"){segment_angle <- 1.377364+0.23271145*pelvic_incidence-0.2307212*pt-0.043317463*l1_pelvic_angle-0.21331655*l1_s1}
  if(t_level == "t8"){segment_angle <- 1.3232688+0.21230806*pelvic_incidence-0.21264924*pt-0.094622739*l1_pelvic_angle-0.21258508*l1_s1}
  if(t_level == "t7"){segment_angle <- -2.783692+0.26196212*pelvic_incidence-0.15382418*pt-0.17593557*l1_pelvic_angle-0.21956343*l1_s1}
  if(t_level == "t6"){segment_angle <- -4.0463956+0.17870291*pelvic_incidence-0.12833362*pt-0.1342015*l1_pelvic_angle-0.14492149*l1_s1}
  if(t_level == "t5"){segment_angle <- -4.4749744+0.11491357*pelvic_incidence-0.073562945*pt-0.13397576*l1_pelvic_angle-0.09637386*l1_s1}
  if(t_level == "t4"){segment_angle <- -5.0083504+0.13101943*pelvic_incidence-0.10157044*pt-0.07159443*l1_pelvic_angle-0.091683749*l1_s1}
  if(t_level == "t3"){segment_angle <- -5.2853366+0.079609721*pelvic_incidence-0.036473976*pt-0.046191883*l1_pelvic_angle-0.054160892*l1_s1}
  if(t_level == "t2"){segment_angle <- -2.0865953+0.0068954812*pelvic_incidence-0.04490273*pt+0.027674194*l1_pelvic_angle-0.002870906*l1_s1}
  if(t_level == "t1"){segment_angle <- 0.87691611+0.049224055*pelvic_incidence-0.0088169242*pt-0.10820582*l1_pelvic_angle-0.030880261*l1_s1}
  return(segment_angle)
}

t12_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {-0.29492722+0.08902534*pelvic_incidence-0.36122518*pt+0.45822599*l1_pelvic_angle-0.12565697*l1_s1+0.12093268*t1_t12 }
t11_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {3.4140574+0.113562*pelvic_incidence-0.32561995*pt+0.31435289*l1_pelvic_angle-0.15874491*l1_s1+0.0095723588*t1_t12 }
t10_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {3.6619563+0.18543994*pelvic_incidence-0.26093372*pt+0.06278895*l1_pelvic_angle-0.19467909*l1_s1-0.042950708*t1_t12 }
t9_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {2.2195784+0.15092012*pelvic_incidence-0.14458981*pt-0.027864799*l1_pelvic_angle-0.13757505*l1_s1-0.053746208*t1_t12 }
t8_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {2.4871069+0.099282369*pelvic_incidence-0.093626111*pt-0.073269033*l1_pelvic_angle-0.10791952*l1_s1-0.074270739*t1_t12 }
t7_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {-1.3902851+0.12664194*pelvic_incidence-0.011323549*pt-0.15036981*l1_pelvic_angle-0.094252421*l1_s1-0.088920758*t1_t12 }
t6_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {-2.0236396-0.017736263*pelvic_incidence+0.078529138*pt-0.097088659*l1_pelvic_angle+0.036987758*l1_s1-0.12908289*t1_t12 }
t5_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {-2.3287076-0.093520316*pelvic_incidence+0.14593099*pt-0.094596787*l1_pelvic_angle+0.096642885*l1_s1-0.13696478*t1_t12 }
t4_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {-2.6879687-0.094323557*pelvic_incidence+0.13572984*pt-0.029020853*l1_pelvic_angle+0.11699139*l1_s1-0.14807599*t1_t12 }
t3_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {-3.4270378-0.10085826*pelvic_incidence+0.15357012*pt-0.012096445*l1_pelvic_angle+0.1129585*l1_s1-0.118588*t1_t12 }
t2_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {-0.123293-0.18376987*pelvic_incidence+0.15587983*pt+0.063696199*l1_pelvic_angle+0.17369159*l1_s1-0.12528883*t1_t12 }
t1_seg_with_t1_t12_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,t1_t12 = 43.586221) {2.3446504-0.093314397*pelvic_incidence+0.14128499*pt-0.08127633*l1_pelvic_angle+0.10111511*l1_s1-0.093663983*t1_t12 }

thoracic_segment_angles_by_tpa_function <- function(thoracic_level = "t12", pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602){
  if(thoracic_level == "t12"){segment_angle <- 0.74830008+0.020483455*pelvic_incidence+0.18482252*pt+0.59044425*l1_pelvic_angle-0.048238157*l1_s1-0.60295017*tpa}
  if(thoracic_level == "t11"){segment_angle <- 2.7052491+0.21543753*pelvic_incidence+0.044859428*pt+0.41535409*l1_pelvic_angle-0.23902378*l1_s1-0.60790221*tpa
  }
  if(thoracic_level == "t10"){segment_angle <- 2.4120385+0.32901847*pelvic_incidence-0.091213981*pt+0.11643504*l1_pelvic_angle-0.3181925*l1_s1-0.40833436*tpa
  }
  if(thoracic_level == "t9"){segment_angle <- 0.87675519+0.30058718*pelvic_incidence-0.023706888*pt+0.01395293*l1_pelvic_angle-0.26797527*l1_s1-0.35435255*tpa
  }
  if(thoracic_level == "t8"){segment_angle <- 1.0547953+0.24870941*pelvic_incidence-0.10162868*pt-0.063908963*l1_pelvic_angle-0.24189823*l1_s1-0.1900372*tpa
  }
  if(thoracic_level == "t7"){segment_angle <- -3.1123853+0.30652845*pelvic_incidence-0.017901238*pt-0.13833257*l1_pelvic_angle-0.25545165*l1_s1-0.23266334*tpa
  }
  if(thoracic_level == "t6"){segment_angle <- -4.2488227+0.20614926*pelvic_incidence-0.044624937*pt-0.11104354*l1_pelvic_angle-0.16702339*l1_s1-0.14328664*tpa
  }
  if(thoracic_level == "t5"){segment_angle <- -4.6389263+0.13714321*pelvic_incidence-0.0057647077*pt-0.11521942*l1_pelvic_angle-0.11427487*l1_s1-0.11605226*tpa
  }
  if(thoracic_level == "t4"){segment_angle<- -5.0181821+0.13235248*pelvic_incidence-0.097504765*pt-0.070469665*l1_pelvic_angle-0.092757223*l1_s1-0.0069593309*tpa
  }
  if(thoracic_level == "t3"){segment_angle <- -5.2220344+0.071026807*pelvic_incidence-0.062651024*pt-0.053433749*l1_pelvic_angle-0.047249273*l1_s1+0.044808031*tpa
  }
  if(thoracic_level == "t2"){segment_angle <- -1.7281239-0.041708352*pelvic_incidence-0.19313965*pt-0.013335467*l1_pelvic_angle+0.036268616*l1_s1+0.25374155*tpa
  }
  if(thoracic_level == "t1"){segment_angle <- 1.2419572-0.00027054105*pelvic_incidence-0.15977059*pt-0.14996707*l1_pelvic_angle+0.0089765709*l1_s1+0.25839188*tpa
  }
  return(segment_angle)
}

# t12_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {0.74830008+0.020483455*pelvic_incidence+0.18482252*pt+0.59044425*l1_pelvic_angle-0.048238157*l1_s1-0.60295017*tpa }
# t11_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {2.7052491+0.21543753*pelvic_incidence+0.044859428*pt+0.41535409*l1_pelvic_angle-0.23902378*l1_s1-0.60790221*tpa }
# t10_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {2.4120385+0.32901847*pelvic_incidence-0.091213981*pt+0.11643504*l1_pelvic_angle-0.3181925*l1_s1-0.40833436*tpa }
# t9_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {0.87675519+0.30058718*pelvic_incidence-0.023706888*pt+0.01395293*l1_pelvic_angle-0.26797527*l1_s1-0.35435255*tpa }
# t8_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {1.0547953+0.24870941*pelvic_incidence-0.10162868*pt-0.063908963*l1_pelvic_angle-0.24189823*l1_s1-0.1900372*tpa }
# t7_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {-3.1123853+0.30652845*pelvic_incidence-0.017901238*pt-0.13833257*l1_pelvic_angle-0.25545165*l1_s1-0.23266334*tpa }
# t6_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {-4.2488227+0.20614926*pelvic_incidence-0.044624937*pt-0.11104354*l1_pelvic_angle-0.16702339*l1_s1-0.14328664*tpa }
# t5_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {-4.6389263+0.13714321*pelvic_incidence-0.0057647077*pt-0.11521942*l1_pelvic_angle-0.11427487*l1_s1-0.11605226*tpa }
# t4_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {-5.0181821+0.13235248*pelvic_incidence-0.097504765*pt-0.070469665*l1_pelvic_angle-0.092757223*l1_s1-0.0069593309*tpa }
# t3_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {-5.2220344+0.071026807*pelvic_incidence-0.062651024*pt-0.053433749*l1_pelvic_angle-0.047249273*l1_s1+0.044808031*tpa }
# t2_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {-1.7281239-0.041708352*pelvic_incidence-0.19313965*pt-0.013335467*l1_pelvic_angle+0.036268616*l1_s1+0.25374155*tpa }
# t1_seg_with_tpa_function <- function(pelvic_incidence = 51.75934,pt = 11.528076,l1_pelvic_angle = 3.8566573,l1_s1 = 61.45634,tpa = 6.2888602) {1.2419572-0.00027054105*pelvic_incidence-0.15977059*pt-0.14996707*l1_pelvic_angle+0.0089765709*l1_s1+0.25839188*tpa }

lumbar_segment_angle_function <- function(l1_s1_input = 39.5,
                                          l1_pelvic_angle_input = 11.222271,
                                          pelvic_incidence_input = 55.435164){ 
  #tried using nonlinear regressions
  # l1_pelvic_angle <- l1_pelvic_angle_input
  # pelvic_incidence <- pelvic_incidence_input
  # l1_s1 <- l1_s1_input
  # 
  # l5_s1 <- 6.7185809+0.75974191* pelvic_incidence+0.00013605755*pmax(pelvic_incidence-34.900559,0)^3-0.001376197*pmax(pelvic_incidence-46.078545,0)^3+0.0024648771*pmax(pelvic_incidence-51.75934,0)^3-0.0013410411*pmax(pelvic_incidence-57.450709,0)^3+0.00011630347*pmax(pelvic_incidence-69.886338,0)^3-0.29348465* l1_s1+0.00035762029*pmax(l1_s1-43.678967,0)^3-0.0029096505*pmax(l1_s1-55.672081,0)^3+0.005755221*pmax(l1_s1-61.45634,0)^3-0.0039797607*pmax(l1_s1-67.329081,0)^3+0.00077656986*pmax(l1_s1-78.067187,0)^3-1.2832397* l1_pelvic_angle-0.0014788224*pmax(l1_pelvic_angle+5.9350206,0)^3+0.0055310962*pmax(l1_pelvic_angle-0.068995368,0)^3-0.0062456079*pmax(l1_pelvic_angle-3.8566573,0)^3+0.0023042547*pmax(l1_pelvic_angle-7.2427704,0)^3-0.00011092059*pmax(l1_pelvic_angle-15.871535,0)^3
  # l5_sa <- l5_s1
  # 
  # l4_l5 <- -6.2175627+1.0422164* pelvic_incidence-5.3979991e-05*pmax(pelvic_incidence-34.900559,0)^3+0.00071173484*pmax(pelvic_incidence-46.078545,0)^3-0.0023697648*pmax(pelvic_incidence-51.75934,0)^3+0.0022435873*pmax(pelvic_incidence-57.450709,0)^3-0.00053157737*pmax(pelvic_incidence-69.886338,0)^3-0.14574479* l1_s1-0.00024034207*pmax(l1_s1-43.678967,0)^3+0.0020949518*pmax(l1_s1-55.672081,0)^3-0.0036171067*pmax(l1_s1-61.45634,0)^3+0.001995834*pmax(l1_s1-67.329081,0)^3-0.00023333695*pmax(l1_s1-78.067187,0)^3-1.9060242* l1_pelvic_angle+0.0030545099*pmax(l1_pelvic_angle+5.9350206,0)^3-0.011753472*pmax(l1_pelvic_angle-0.068995368,0)^3+0.014604222*pmax(l1_pelvic_angle-3.8566573,0)^3-0.0065295069*pmax(l1_pelvic_angle-7.2427704,0)^3+0.00062424662*pmax(l1_pelvic_angle-15.871535,0)^3-0.68969508* l5_s1+0.00017754685*pmax(l5_s1-11.329631,0)^3-0.001952767*pmax(l5_s1-19.669301,0)^3+0.0038235367*pmax(l5_s1-23.962803,0)^3-0.0019194178*pmax(l5_s1-26.609556,0)^3-0.00012889877*pmax(l5_s1-32.194105,0)^3
  # l4_sa <- l4_l5
  # 
  # l3_l4 <- -0.74880205+0.70469788* pelvic_incidence-0.00021078294*pmax(pelvic_incidence-34.900559,0)^3+0.0015072337*pmax(pelvic_incidence-46.078545,0)^3-0.002829365*pmax(pelvic_incidence-51.75934,0)^3+0.0018317041*pmax(pelvic_incidence-57.450709,0)^3-0.00029878985*pmax(pelvic_incidence-69.886338,0)^3+0.14012568* l1_s1-0.00053736888*pmax(l1_s1-43.678967,0)^3+0.0042802121*pmax(l1_s1-55.672081,0)^3-0.0072065532*pmax(l1_s1-61.45634,0)^3+0.0039420649*pmax(l1_s1-67.329081,0)^3-0.00047835497*pmax(l1_s1-78.067187,0)^3-1.2487701* l1_pelvic_angle+0.0016161456*pmax(l1_pelvic_angle+5.9350206,0)^3-0.0052142788*pmax(l1_pelvic_angle-0.068995368,0)^3+0.0037560169*pmax(l1_pelvic_angle-3.8566573,0)^3+0.00023505049*pmax(l1_pelvic_angle-7.2427704,0)^3-0.00039293418*pmax(l1_pelvic_angle-15.871535,0)^3-0.93019505* l5_s1+0.0012337248*pmax(l5_s1-11.329631,0)^3-0.0057844781*pmax(l5_s1-19.669301,0)^3+0.0060114369*pmax(l5_s1-23.962803,0)^3-0.00049664111*pmax(l5_s1-26.609556,0)^3-0.00096404249*pmax(l5_s1-32.194105,0)^3-0.43017745* l4_l5-0.000677315*pmax(l4_l5-8.6858167,0)^3+0.003990904*pmax(l4_l5-13.885879,0)^3-0.0086836739*pmax(l4_l5-16.604356,0)^3+0.0071133048*pmax(l4_l5-19.460103,0)^3-0.0017432199*pmax(l4_l5-25.11043,0)^3
  # l3_sa <- l3_l4
  # 
  # l2_l3 <- 1.8777349+0.1859238* pelvic_incidence+8.9839032e-05*pmax(pelvic_incidence-34.900559,0)^3-0.00038169268*pmax(pelvic_incidence-46.078545,0)^3+0.00054273955*pmax(pelvic_incidence-51.75934,0)^3-0.00031313794*pmax(pelvic_incidence-57.450709,0)^3+6.2252037e-05*pmax(pelvic_incidence-69.886338,0)^3+0.45814027* l1_s1-0.00013112519*pmax(l1_s1-43.678967,0)^3+0.0012366409*pmax(l1_s1-55.672081,0)^3-0.002382885*pmax(l1_s1-61.45634,0)^3+0.0015269172*pmax(l1_s1-67.329081,0)^3-0.00024954781*pmax(l1_s1-78.067187,0)^3-0.47317543* l1_pelvic_angle+0.00042489951*pmax(l1_pelvic_angle+5.9350206,0)^3-0.0012940269*pmax(l1_pelvic_angle-0.068995368,0)^3+0.0017783674*pmax(l1_pelvic_angle-3.8566573,0)^3-0.0011801864*pmax(l1_pelvic_angle-7.2427704,0)^3+0.00027094638*pmax(l1_pelvic_angle-15.871535,0)^3-0.66514353* l5_s1+0.000160417*pmax(l5_s1-11.329631,0)^3-0.00023645481*pmax(l5_s1-19.669301,0)^3-0.0020608003*pmax(l5_s1-23.962803,0)^3+0.0029684766*pmax(l5_s1-26.609556,0)^3-0.00083163846*pmax(l5_s1-32.194105,0)^3-0.53051132* l4_l5-0.0015872266*pmax(l4_l5-8.6858167,0)^3+0.0065980488*pmax(l4_l5-13.885879,0)^3-0.0057140727*pmax(l4_l5-16.604356,0)^3+0.00010862597*pmax(l4_l5-19.460103,0)^3+0.00059462457*pmax(l4_l5-25.11043,0)^3-0.33540128* l3_l4-0.010134845*pmax(l3_l4-5.7123008,0)^3+0.059183228*pmax(l3_l4-8.99933,0)^3-0.082517259*pmax(l3_l4-10.829323,0)^3+0.038860501*pmax(l3_l4-13.269923,0)^3-0.0053916257*pmax(l3_l4-17.951153,0)^3
  # l2_sa <- l2_l3
  # 
  # l1_sa <- l1_s1 - (l5_s1 + l4_l5 + l3_l4 + l2_l3)
  
  ## this worked OK:
  l5_sa <- 2.028759-0.2004133*l1_s1_input-1.2948269*l1_pelvic_angle_input+0.72916193*pelvic_incidence_input

  l4_sa <- -4.2323446-0.14264788*l1_s1_input-0.66483692*l5_sa-1.6659845*l1_pelvic_angle_input+1.0011084*pelvic_incidence_input

  l3_sa <- -5.6312247+0.098489688*l1_s1_input-0.91616568*l5_sa-0.53156915*l4_sa-1.4696836*l1_pelvic_angle_input+0.92315489*pelvic_incidence_input

  l2_sa <- -2.3078228+0.47785577*l1_s1_input-0.80732669*l5_sa-0.73245566*l4_sa-0.51444405*l3_sa-0.62688324*l1_pelvic_angle_input+0.38754186*pelvic_incidence_input

  l1_sa <- 1.4066407e-13+1*l1_s1_input-1*l5_sa-1*l4_sa-1*l3_sa-1*l2_sa-5.0984901e-16*l1_pelvic_angle_input+2.1368709e-16*pelvic_incidence_input
  
  lumbar_segment_angles_list <- list(l5_segment_angle = l5_sa,
                                     l4_segment_angle = l4_sa,
                                     l3_segment_angle = l3_sa,
                                     l2_segment_angle = l2_sa,
                                     l1_segment_angle = l1_sa)
  return(lumbar_segment_angles_list)
}

thoracic_segment_angles_function <- function(l1_s1_input = 39.5,l1_pelvic_angle_input = 11.222271,pelvic_incidence_input = 55.435164,t4_t12_input = -32.7,t4_pelvic_angle_input = 19.369452) {
  t4_t12_input <- t4_t12_input*-1
  t12_sa <- -1.2432187-0.56338566*l1_s1_input+0.87843878*l1_pelvic_angle_input+0.5103392*pelvic_incidence_input-0.21307175*t4_t12_input-1.3178337*t4_pelvic_angle_input
  
  t11_sa <- -0.83916975-0.29121935*t12_sa-0.61968425*l1_s1_input+0.71116474*l1_pelvic_angle_input+0.62515338*pelvic_incidence_input-0.10468428*t4_t12_input-1.3168397*t4_pelvic_angle_input
  
  t10_sa <- 0.82052592-0.11383187*t11_sa-0.44191482*t12_sa-0.5843131*l1_s1_input+0.62310746*l1_pelvic_angle_input+0.58612084*pelvic_incidence_input-0.054484117*t4_t12_input-1.2046572*t4_pelvic_angle_input
  
  t9_sa <- 0.44589952-0.027082825*t10_sa-0.29467934*t11_sa-0.4252468*t12_sa-0.44179798*l1_s1_input+0.46525383*l1_pelvic_angle_input+0.46289389*pelvic_incidence_input+0.027649066*t4_t12_input-0.91085339*t4_pelvic_angle_input
  
  t8_sa <- -0.76354758+0.079321328*t9_sa-0.33399877*t10_sa-0.38545404*t11_sa-0.41123736*t12_sa-0.43370819*l1_s1_input+0.44832215*l1_pelvic_angle_input+0.45705009*pelvic_incidence_input+0.066780019*t4_t12_input-0.882922*t4_pelvic_angle_input
  
  t7_sa <- -0.36947144-0.048194619*t8_sa-0.23318337*t9_sa-0.39080782*t10_sa-0.4541907*t11_sa-0.41299988*t12_sa-0.36722696*l1_s1_input+0.42801275*l1_pelvic_angle_input+0.3722355*pelvic_incidence_input+0.14692544*t4_t12_input-0.77428477*t4_pelvic_angle_input
  
  t6_sa <- -1.0883547-0.07454856*t7_sa-0.23280179*t8_sa-0.30433902*t9_sa-0.3709043*t10_sa-0.43049871*t11_sa-0.3236001*t12_sa-0.22309461*l1_s1_input+0.25654313*l1_pelvic_angle_input+0.2321191*pelvic_incidence_input+0.22127623*t4_t12_input-0.46755303*t4_pelvic_angle_input
  
  t5_sa <- 0.69162214-0.31975675*t6_sa-0.4412255*t7_sa-0.50623331*t8_sa-0.53913213*t9_sa-0.57230033*t10_sa-0.65197163*t11_sa-0.48260958*t12_sa-0.29316955*l1_s1_input+0.37372636*l1_pelvic_angle_input+0.27267218*pelvic_incidence_input+0.39331364*t4_t12_input-0.63336495*t4_pelvic_angle_input
  
  t4_sa <- -0.28175955-0.56942085*t5_sa-0.63585333*t6_sa-0.69326886*t7_sa-0.77121564*t8_sa-0.72964636*t9_sa-0.78017101*t10_sa-0.81672537*t11_sa-0.48154746*t12_sa-0.14032371*l1_s1_input+0.17613688*l1_pelvic_angle_input+0.13210963*pelvic_incidence_input+0.65367631*t4_t12_input-0.28707275*t4_pelvic_angle_input
  
  t3_sa <- -2.5099283+0.32644445*t4_sa+0.010587101*t5_sa-0.09069735*t6_sa+0.028017449*t7_sa+0.024184383*t8_sa-0.051232677*t9_sa+0.014130663*t10_sa+0.043587369*t11_sa+0.048197154*t12_sa+0.087459387*l1_s1_input-0.18412973*l1_pelvic_angle_input-0.070866287*pelvic_incidence_input+0.035919594*t4_t12_input+0.25213504*t4_pelvic_angle_input
  
  t2_sa <- -1.1310498+0.35405136*t3_sa+0.023685093*t4_sa-0.066344556*t5_sa-0.011666527*t6_sa-0.06165322*t7_sa-0.047877164*t8_sa-0.023500339*t9_sa-0.02582784*t10_sa+0.018402494*t11_sa+0.029379976*t12_sa+0.042692784*l1_s1_input-0.10099911*l1_pelvic_angle_input-0.053272235*pelvic_incidence_input+0.012689279*t4_t12_input+0.15739916*t4_pelvic_angle_input
  
  t1_sa <- -0.63351857+0.38606222*t2_sa-0.024390745*t3_sa-0.017735187*t4_sa-0.09823423*t5_sa-0.055845049*t6_sa-0.13693589*t7_sa+0.019874629*t8_sa-0.060870147*t9_sa-0.0083761595*t10_sa-0.022734773*t11_sa+0.042125893*t12_sa+0.016763235*l1_s1_input-0.071824176*l1_pelvic_angle_input-0.040060676*pelvic_incidence_input+0.011215865*t4_t12_input+0.11641339*t4_pelvic_angle_input
  
  return(list(t12_segment_angle = t12_sa,
              t11_segment_angle = t11_sa,
              t10_segment_angle = t10_sa,
              t9_segment_angle = t9_sa,
              t8_segment_angle = t8_sa,
              t7_segment_angle = t7_sa,
              t6_segment_angle = t6_sa,
              t5_segment_angle = t5_sa,
              t4_segment_angle = t4_sa,
              t3_segment_angle = t3_sa,
              t2_segment_angle = t2_sa,
              t1_segment_angle = t1_sa))
}

initial_segment_angle_function_using_lpa_tpa <- function(l1_pelvic_angle_input = 10,
                                                 pelvic_incidence_input = 50,
                                                 l1_s1_lordosis_input = 50,
                                                 pt_input = 20,
                                                 t4_t12_input = 25,
                                                 t4_pelvic_angle_input = 10
){
  
  lumbar_segment_angles_list <- lumbar_segment_angle_function(l1_s1_input = l1_s1_lordosis_input, l1_pelvic_angle_input = l1_pelvic_angle_input, pelvic_incidence_input = pelvic_incidence_input)
  
  thoracic_segment_angles_list <- thoracic_segment_angles_function(l1_s1_input = l1_s1_lordosis_input, l1_pelvic_angle_input = l1_pelvic_angle_input, pelvic_incidence_input = pelvic_incidence_input, t4_t12_input = t4_t12_input, t4_pelvic_angle_input = t4_pelvic_angle_input)
  
  l1_segment_angle <- lumbar_segment_angles_list$l1_segment_angle
  l2_segment_angle <- lumbar_segment_angles_list$l2_segment_angle
  l3_segment_angle <- lumbar_segment_angles_list$l3_segment_angle
  l4_segment_angle <- lumbar_segment_angles_list$l4_segment_angle
  l5_segment_angle <- lumbar_segment_angles_list$l5_segment_angle
  
  segment_angle_list <-
    list(
      l5_segment_angle = l5_segment_angle,
      l4_segment_angle = l4_segment_angle,
      l3_segment_angle = l3_segment_angle,
      l2_segment_angle = l2_segment_angle,
      l1_segment_angle = l1_segment_angle,
      t12_segment_angle = thoracic_segment_angles_list$t12_segment_angle,
      t11_segment_angle = thoracic_segment_angles_list$t11_segment_angle,
      t10_segment_angle = thoracic_segment_angles_list$t10_segment_angle,
      t9_segment_angle = thoracic_segment_angles_list$t9_segment_angle,
      t8_segment_angle = thoracic_segment_angles_list$t8_segment_angle,
      t7_segment_angle = thoracic_segment_angles_list$t7_segment_angle,
      t6_segment_angle = thoracic_segment_angles_list$t6_segment_angle,
      t5_segment_angle = thoracic_segment_angles_list$t5_segment_angle,
      t4_segment_angle = thoracic_segment_angles_list$t4_segment_angle,
      t3_segment_angle = thoracic_segment_angles_list$t3_segment_angle,
      t2_segment_angle = thoracic_segment_angles_list$t2_segment_angle,
      t1_segment_angle = thoracic_segment_angles_list$t1_segment_angle
    )
  
  return(segment_angle_list = segment_angle_list)
  
}

segment_angle_function_using_lpa_tpa <- function(l1_pelvic_angle_input = 10,
                                                 pelvic_incidence_input = 50,
                                                 l1_s1_lordosis_input = 50,
                                                 pt_input = 20,
                                                 t4_t12_input = 25,
                                                 t4_pelvic_angle_input = 10
){
  
  initial_segment_angles_list <- initial_segment_angle_function_using_lpa_tpa(l1_pelvic_angle_input = l1_pelvic_angle_input, 
                                               pelvic_incidence_input = pelvic_incidence_input, 
                                               l1_s1_lordosis_input = l1_s1_lordosis_input, 
                                               pt_input = pt_input, 
                                               t4_t12_input = t4_t12_input,
                                               t4_pelvic_angle_input = t4_pelvic_angle_input)
  
  initial_measured_pelvic_angles_preop_list <- jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = pelvic_incidence_input,
                                                                                     segment_angle_list_start = initial_segment_angles_list, 
                                                                                     cervical_lordosis_start = 20)
  
  while(round(initial_measured_pelvic_angles_preop_list$l1pa_value) > l1_pelvic_angle_input){
    initial_segment_angles_list$l1_segment_angle <- initial_segment_angles_list$l1_segment_angle - 1
    initial_segment_angles_list$l5_segment_angle <- initial_segment_angles_list$l5_segment_angle + 1

    initial_measured_pelvic_angles_preop_list <- jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = pelvic_incidence_input,
                                                                                          segment_angle_list_start = initial_segment_angles_list,
                                                                                          cervical_lordosis_start = 20)
  }
  
  while(round(initial_measured_pelvic_angles_preop_list$t4pa_value) > t4_pelvic_angle_input){
    initial_segment_angles_list$t12_segment_angle <- initial_segment_angles_list$t12_segment_angle + 1
    initial_segment_angles_list$t11_segment_angle <- initial_segment_angles_list$t11_segment_angle + 1
    initial_segment_angles_list$t4_segment_angle <- initial_segment_angles_list$t4_segment_angle - 1
    initial_segment_angles_list$t5_segment_angle <- initial_segment_angles_list$t5_segment_angle - 1

    initial_measured_pelvic_angles_preop_list <- jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = pelvic_incidence_input,
                                                                                          segment_angle_list_start = initial_segment_angles_list,
                                                                                          cervical_lordosis_start = 20)
  }
  
  segment_angle_list <- initial_segment_angles_list
  
  # lumbar_segment_angles_list <- lumbar_segment_angle_function(l1_s1_input = l1_s1_lordosis_input, l1_pelvic_angle_input = l1_pelvic_angle_input, pelvic_incidence_input = pelvic_incidence_input)
  # 
  # thoracic_segment_angles_list <- thoracic_segment_angles_function(l1_s1_input = l1_s1_lordosis_input, l1_pelvic_angle_input = l1_pelvic_angle_input, pelvic_incidence_input = pelvic_incidence_input, t4_t12_input = t4_t12_input, t4_pelvic_angle_input = t4_pelvic_angle_input)
  # 
  # l1_segment_angle <- lumbar_segment_angles_list$l1_segment_angle
  # l2_segment_angle <- lumbar_segment_angles_list$l2_segment_angle
  # l3_segment_angle <- lumbar_segment_angles_list$l3_segment_angle
  # l4_segment_angle <- lumbar_segment_angles_list$l4_segment_angle
  # l5_segment_angle <- lumbar_segment_angles_list$l5_segment_angle
  # 
  # segment_angle_list <-
  #   list(
  #     l5_segment_angle = l5_segment_angle,
  #     l4_segment_angle = l4_segment_angle,
  #     l3_segment_angle = l3_segment_angle,
  #     l2_segment_angle = l2_segment_angle,
  #     l1_segment_angle = l1_segment_angle,
  #     t12_segment_angle = thoracic_segment_angles_list$t12_segment_angle,
  #     t11_segment_angle = thoracic_segment_angles_list$t11_segment_angle,
  #     t10_segment_angle = thoracic_segment_angles_list$t10_segment_angle,
  #     t9_segment_angle = thoracic_segment_angles_list$t9_segment_angle,
  #     t8_segment_angle = thoracic_segment_angles_list$t8_segment_angle,
  #     t7_segment_angle = thoracic_segment_angles_list$t7_segment_angle,
  #     t6_segment_angle = thoracic_segment_angles_list$t6_segment_angle,
  #     t5_segment_angle = thoracic_segment_angles_list$t5_segment_angle,
  #     t4_segment_angle = thoracic_segment_angles_list$t4_segment_angle,
  #     t3_segment_angle = thoracic_segment_angles_list$t3_segment_angle,
  #     t2_segment_angle = thoracic_segment_angles_list$t2_segment_angle,
  #     t1_segment_angle = thoracic_segment_angles_list$t1_segment_angle
  #   )
  
  return(segment_angle_list = segment_angle_list)
  
}


######## TARGET FUNCTIONS
lumbar_segment_angle_target_function <- function(pelvic_incidence_input = 55.435164,
                                                 fixed_l5sa = 99,
                                                 fixed_l4sa = 99, 
                                                 fixed_l3sa = 99,
                                                 fixed_l2sa = 99, 
                                                 fixed_l1sa = 99){ 
  
  l1_pelvic_angle_input <- 0.5*pelvic_incidence_input - 19
  
  l1_s1_input <- 1.5*pelvic_incidence_input - 1.7*l1_pelvic_angle_input - 2
  
  if(fixed_l5sa == 99){
    l5_sa <- 2.028759-0.2004133*l1_s1_input-1.2948269*l1_pelvic_angle_input+0.72916193*pelvic_incidence_input 
    if(l5_sa > 34){l5_sa <- 34}
  }else{
    l5_sa <-fixed_l5sa
  }
  if(fixed_l4sa == 99){
    l4_sa <- -4.2323446-0.14264788*l1_s1_input-0.66483692*l5_sa-1.6659845*l1_pelvic_angle_input+1.0011084*pelvic_incidence_input
    if(l4_sa > 34){l4_sa <- 34}
  }else{
    l4_sa <-fixed_l4sa
  }
  if(fixed_l3sa == 99){
    l3_sa <- -5.6312247+0.098489688*l1_s1_input-0.91616568*l5_sa-0.53156915*l4_sa-1.4696836*l1_pelvic_angle_input+0.92315489*pelvic_incidence_input
    if(l3_sa > 30){l3_sa <- 30}
  }else{
    l3_sa <-fixed_l3sa
  }
  if(fixed_l2sa == 99){
    l2_sa <- -2.3078228+0.47785577*l1_s1_input-0.80732669*l5_sa-0.73245566*l4_sa-0.51444405*l3_sa-0.62688324*l1_pelvic_angle_input+0.38754186*pelvic_incidence_input
    if(l2_sa > 20){l2_sa <- 20}
  }else{
    l2_sa <- fixed_l2sa
  }
  if(fixed_l1sa == 99){
    l1_sa <- 1.4066407e-13+1*l1_s1_input-1*l5_sa-1*l4_sa-1*l3_sa-1*l2_sa-5.0984901e-16*l1_pelvic_angle_input+2.1368709e-16*pelvic_incidence_input
    if(l1_sa > 15){l1_sa <- 15}
  }else{
    l1_sa <-fixed_l1sa
  }
  
  lumbar_segment_angles_list <- list(l5_segment_angle = l5_sa,
                                     l4_segment_angle = l4_sa,
                                     l3_segment_angle = l3_sa,
                                     l2_segment_angle = l2_sa,
                                     l1_segment_angle = l1_sa)
  return(lumbar_segment_angles_list)
}

thoracic_segment_angles_target_function <- function(pelvic_incidence_input = 55.435164,
                                                    fixed_t12sa = 99,
                                                    fixed_t11sa = 99, 
                                                    fixed_t10sa = 99
                                                    ) {
  # t4_t12_input <- t4_t12_input*-1
  l1_pelvic_angle_input <- 0.5*pelvic_incidence_input - 20
  
  l1_s1_input <- 1.5*pelvic_incidence_input - 1.7*l1_pelvic_angle_input - 2
  
  t4_pelvic_angle_input <- 4.9128149+1.0184181*l1_pelvic_angle_input-0.043355873*pelvic_incidence_input
  
  t4_t12_input <- -23.303807+1.5966063*pelvic_incidence_input-1.5812637*l1_s1_input+0.80865399*l1_pelvic_angle_input-2.5347033*t4_pelvic_angle_input
  
  if(fixed_t12sa == 99){
    t12_sa <- -0.444323+0.50366706*pelvic_incidence_input-0.5389336*l1_s1_input+0.80111694*l1_pelvic_angle_input-1.2364157*t4_pelvic_angle_input-0.16953639*t4_t12_input
  }else{
    t12_sa <- fixed_t12sa
  }
  
  if(fixed_t11sa == 99){
    t11_sa <- -0.36289065-0.29030277*t12_sa+0.53338008*pelvic_incidence_input-0.53546178*l1_s1_input+0.65785618*l1_pelvic_angle_input-1.1736363*t4_pelvic_angle_input-0.075942774*t4_t12_input
  }else{
    t11_sa <- fixed_t11sa
  }
  if(fixed_t10sa == 99){
    t10_sa <- 0.31053626-0.317766*t11_sa-0.52225237*t12_sa+0.6806195*pelvic_incidence_input-0.67008048*l1_s1_input+0.74149873*l1_pelvic_angle_input-1.4329479*t4_pelvic_angle_input-0.060764256*t4_t12_input
  }else{
    t10_sa <- fixed_t10sa
  }
  t9_sa <- 0.77613769-0.27406719*t10_sa-0.41828452*t11_sa-0.59986469*t12_sa+0.67787432*pelvic_incidence_input-0.67039588*l1_s1_input+0.73494439*l1_pelvic_angle_input-1.3993777*t4_pelvic_angle_input-0.012979952*t4_t12_input
  
  t8_sa <- -0.36991442-0.11462775*t9_sa-0.34697405*t10_sa-0.44849026*t11_sa-0.5226705*t12_sa+0.56614797*pelvic_incidence_input-0.54792467*l1_s1_input+0.57908889*l1_pelvic_angle_input-1.1426969*t4_pelvic_angle_input+0.049862183*t4_t12_input
  
  t7_sa <- -0.48799997-0.081013076*t8_sa-0.25333867*t9_sa-0.41343939*t10_sa-0.44408285*t11_sa-0.4712475*t12_sa+0.43764785*pelvic_incidence_input-0.43448707*l1_s1_input+0.51473869*l1_pelvic_angle_input-0.93707461*t4_pelvic_angle_input+0.1171369*t4_t12_input
  
  t6_sa <- -0.047224514-0.1443496*t7_sa-0.2647053*t8_sa-0.38555157*t9_sa-0.45010894*t10_sa-0.5331795*t11_sa-0.43636409*t12_sa+0.37023041*pelvic_incidence_input-0.37686936*l1_s1_input+0.43702635*l1_pelvic_angle_input-0.80303312*t4_pelvic_angle_input+0.20561717*t4_t12_input
  
  t5_sa <- 0.73333549-0.40805036*t6_sa-0.55060343*t7_sa-0.58980254*t8_sa-0.68516685*t9_sa-0.7230473*t10_sa-0.78182429*t11_sa-0.66241598*t12_sa+0.48761616*pelvic_incidence_input-0.49907732*l1_s1_input+0.59746534*l1_pelvic_angle_input-1.0874566*t4_pelvic_angle_input+0.40689771*t4_t12_input
  
  t4_sa <- -0.53009188-0.63654572*t5_sa-0.6701609*t6_sa-0.74495885*t7_sa-0.82467189*t8_sa-0.86484065*t9_sa-0.83754838*t10_sa-0.95435598*t11_sa-0.59605027*t12_sa+0.2955364*pelvic_incidence_input-0.2915725*l1_s1_input+0.26640972*l1_pelvic_angle_input-0.54442006*t4_pelvic_angle_input+0.66903811*t4_t12_input 
  
  t3_sa <- -3.3766159+0.44140992*t4_sa+0.047717107*t5_sa+0.02976592*t6_sa+0.04834378*t7_sa+0.17874007*t8_sa+0.08146022*t9_sa+0.14638872*t10_sa+0.2752716*t11_sa+0.21104871*t12_sa-0.26810551*pelvic_incidence_input+0.25627922*l1_s1_input-0.35630641*l1_pelvic_angle_input+0.65354973*t4_pelvic_angle_input-0.046073831*t4_t12_input
  
  t2_sa <- -1.82413+0.3662619*t3_sa+0.045964653*t4_sa-0.095964338*t5_sa-0.15587649*t6_sa-0.094395385*t7_sa-0.081327455*t8_sa-0.13483071*t9_sa-0.11415574*t10_sa-0.1512905*t11_sa-0.13393218*t12_sa+0.13808844*pelvic_incidence_input-0.14351391*l1_s1_input+0.032364048*l1_pelvic_angle_input-0.17366499*t4_pelvic_angle_input+0.024095196*t4_t12_input
  
  t1_sa <- -1.8403414+0.30874234*t2_sa+0.019712122*t3_sa-0.078022673*t4_sa-0.020075934*t5_sa-0.077796258*t6_sa-0.053105214*t7_sa-0.011712084*t8_sa-0.043866693*t9_sa-0.046003646*t10_sa-0.061735571*t11_sa-0.027094892*t12_sa+0.040942215*pelvic_incidence_input-0.063883486*l1_s1_input+0.0055270493*l1_pelvic_angle_input-0.033277089*t4_pelvic_angle_input-0.03426909*t4_t12_input 
  
 
  return(list(t12_segment_angle = t12_sa,
              t11_segment_angle = t11_sa,
              t10_segment_angle = t10_sa,
              t9_segment_angle = t9_sa,
              t8_segment_angle = t8_sa,
              t7_segment_angle = t7_sa,
              t6_segment_angle = t6_sa,
              t5_segment_angle = t5_sa,
              t4_segment_angle = t4_sa,
              t3_segment_angle = t3_sa,
              t2_segment_angle = t2_sa,
              t1_segment_angle = t1_sa))
}


segment_angle_function_targets <- function(pelvic_incidence_input = 50,
                                           fixed_l5sa_input = 99,
                                           fixed_l4sa_input = 99,
                                           fixed_l3sa_input = 99,
                                           fixed_l2sa_input = 99,
                                           fixed_l1sa_input = 99,
                                           fixed_t12sa_input = 99,
                                           fixed_t11sa_input = 99,
                                           fixed_t10sa_input = 99
){
  
  lumbar_segment_angles_list <- lumbar_segment_angle_target_function(pelvic_incidence_input = pelvic_incidence_input, 
                                                                     fixed_l5sa = fixed_l5sa_input, 
                                                                     fixed_l4sa = fixed_l4sa_input,
                                                                     fixed_l3sa = fixed_l3sa_input,
                                                                     fixed_l2sa = fixed_l2sa_input,
                                                                     fixed_l1sa = fixed_l1sa_input)
  
  thoracic_segment_angles_list <-thoracic_segment_angles_target_function(pelvic_incidence_input = pelvic_incidence_input, 
                                                                         fixed_t12sa = fixed_t12sa_input, 
                                                                         fixed_t11sa = fixed_t11sa_input, 
                                                                         fixed_t10sa = fixed_t10sa_input)
  
  # thoracic_segment_angles_list <- thoracic_segment_angles_function(l1_s1_input = l1_s1_lordosis_input, l1_pelvic_angle_input = l1_pelvic_angle_input, pelvic_incidence_input = pelvic_incidence_input, t4_t12_input = t4_t12_input, t4_pelvic_angle_input = t4_pelvic_angle_input)
  
  l1_segment_angle <- lumbar_segment_angles_list$l1_segment_angle
  l2_segment_angle <- lumbar_segment_angles_list$l2_segment_angle
  l3_segment_angle <- lumbar_segment_angles_list$l3_segment_angle
  l4_segment_angle <- lumbar_segment_angles_list$l4_segment_angle
  l5_segment_angle <- lumbar_segment_angles_list$l5_segment_angle
  
  segment_angle_list <-
    list(
      l5_segment_angle = l5_segment_angle,
      l4_segment_angle = l4_segment_angle,
      l3_segment_angle = l3_segment_angle,
      l2_segment_angle = l2_segment_angle,
      l1_segment_angle = l1_segment_angle,
      t12_segment_angle = thoracic_segment_angles_list$t12_segment_angle,
      t11_segment_angle = thoracic_segment_angles_list$t11_segment_angle,
      t10_segment_angle = thoracic_segment_angles_list$t10_segment_angle,
      t9_segment_angle = thoracic_segment_angles_list$t9_segment_angle,
      t8_segment_angle = thoracic_segment_angles_list$t8_segment_angle,
      t7_segment_angle = thoracic_segment_angles_list$t7_segment_angle,
      t6_segment_angle = thoracic_segment_angles_list$t6_segment_angle,
      t5_segment_angle = thoracic_segment_angles_list$t5_segment_angle,
      t4_segment_angle = thoracic_segment_angles_list$t4_segment_angle,
      t3_segment_angle = thoracic_segment_angles_list$t3_segment_angle,
      t2_segment_angle = thoracic_segment_angles_list$t2_segment_angle,
      t1_segment_angle = thoracic_segment_angles_list$t1_segment_angle
    )
  
  return(segment_angle_list = segment_angle_list)
  
}

################# OLD ##########

segment_angle_function_using_lpa <- function(l1pa = 10,
                                             pelvic_incidence = 50,
                                             l1_s1_lordosis = 50,
                                             spinal_kyphosis = 45,
                                             tk_upper = 10, 
                                             tk_lower = 1, 
                                             add_tk = 0, 
                                             lumbar_all_combinations_df = NULL
){
  
  tk_range <- tk_upper-tk_lower
  
  tk <- -1 * spinal_kyphosis / tk_range
  
  thoracic_lordosis_range <- tk_lower  ## this will add lordosis evenly under wherever thoracic kyphosis starts
  
  if (thoracic_lordosis_range > 0) {
    add_thoracic_lordosis <- add_tk / tk_range
  } else {
    add_thoracic_lordosis <- 0
  }
  
  # pelvic_incidence_input <- round(pelvic_incidence, 0)
  # l1_s1_lordosis_input <- round(l1_s1_lordosis, 0)
  # l1pa <- round(l1pa, 0)
  # 
  # lumbar_segment_angles_df <- lumbar_all_combinations_df %>%
  #   filter(pelvic_incidence == pelvic_incidence,
  #          l1_s1 == l1_s1_lordosis,
  #          lpa == l1pa)
  
  lumbar_segment_angles_df <- lumbar_all_combinations_df
  
  lumbar_segment_angles_list <- lumbar_segment_angle_function(l1_s1_input = l1_s1_lordosis,
                                                              l1_pelvic_angle_input = l1pa, 
                                                              pelvic_incidence_input = pelvic_incidence)
  
  l1_segment_angle <- as.double(lumbar_segment_angles_list$l1_segment_angle)
  l2_segment_angle <- as.double(lumbar_segment_angles_list$l2_segment_angle)
  l3_segment_angle <- as.double(lumbar_segment_angles_list$l3_segment_angle)
  l4_segment_angle <- as.double(lumbar_segment_angles_list$l4_segment_angle)
  l5_segment_angle <- as.double(lumbar_segment_angles_list$l5_segment_angle)
  
  # l1_segment_angle <- as.double(lumbar_segment_angles_df$l1_segment_angle)
  # l2_segment_angle <- as.double(lumbar_segment_angles_df$l2_segment_angle)
  # l3_segment_angle <- as.double(lumbar_segment_angles_df$l3_segment_angle)
  # l4_segment_angle <- as.double(lumbar_segment_angles_df$l4_segment_angle)
  # l5_segment_angle <- as.double(lumbar_segment_angles_df$l5_segment_angle)
  
  
  t12_segment_angle <- if_else(tk_lower <= 0, tk, add_thoracic_lordosis)
  t11_segment_angle <- if_else(tk_lower <= 1 & tk_upper >= 1, tk, add_thoracic_lordosis)
  t10_segment_angle <- if_else(tk_lower <= 2 & tk_upper >= 2, tk, add_thoracic_lordosis) #9 on uiv
  t9_segment_angle <-  if_else(tk_lower <= 3 & tk_upper >= 3, tk, 0) #10 on uiv
  t8_segment_angle <-  if_else(tk_lower <= 4 & tk_upper >= 4, tk, 0)
  t7_segment_angle <-  if_else(tk_lower <= 5 & tk_upper >= 5, tk, 0)
  t6_segment_angle <-  if_else(tk_lower <= 6 & tk_upper >= 6, tk, 0)
  t5_segment_angle <-  if_else(tk_lower <= 7 & tk_upper >= 7, tk, 0)
  t4_segment_angle <-  if_else(tk_lower <= 8 & tk_upper >= 8, tk, 0)
  t3_segment_angle <-  if_else(tk_lower <= 9 & tk_upper >= 9, tk, 0)
  t2_segment_angle <-  if_else(tk_lower <= 10 & tk_upper >= 10, tk, 0)
  t1_segment_angle <-  if_else(tk_lower <= 11 & tk_upper >= 11, tk, 0)
  # }
  
  segment_angle_list <-
    list(
      l5_segment_angle = l5_segment_angle,
      l4_segment_angle = l4_segment_angle,
      l3_segment_angle = l3_segment_angle,
      l2_segment_angle = l2_segment_angle,
      l1_segment_angle = l1_segment_angle,
      t12_segment_angle = t12_segment_angle,
      t11_segment_angle = t11_segment_angle,
      t10_segment_angle = t10_segment_angle,
      t9_segment_angle = t9_segment_angle,
      t8_segment_angle = t8_segment_angle,
      t7_segment_angle = t7_segment_angle,
      t6_segment_angle = t6_segment_angle,
      t5_segment_angle = t5_segment_angle,
      t4_segment_angle = t4_segment_angle,
      t3_segment_angle = t3_segment_angle,
      t2_segment_angle = t2_segment_angle,
      t1_segment_angle = t1_segment_angle
    )
  
  return(segment_angle_list = segment_angle_list)
  
}

segment_angle_function_using_l4s1 <- function(pelvic_incidence,
                                              l1_s1_lordosis = 50,
                                              l4_s1_lordosis = 30,
                                              spinal_kyphosis = 45,
                                              inflection = "T12",
                                              tk_upper = 10, 
                                              tk_lower = 1, 
                                              add_tk = 0
){
  
  tk_range <- tk_upper-tk_lower
  
  tk <- -1 * spinal_kyphosis / tk_range
  
  thoracic_lordosis_range <- tk_lower  ## this will add lordosis evenly under wherever thoracic kyphosis starts
  
  if (thoracic_lordosis_range > 0) {
    add_thoracic_lordosis <- add_tk / tk_range
  } else {
    add_thoracic_lordosis <- 0
  }
  
  pelvic_incidence <- round(pelvic_incidence, 0)
  l1_s1_lordosis <- round(l1_s1_lordosis, 0)
  
  l5_segment_angle <- l4_s1_lordosis/2
  l4_segment_angle <- l4_s1_lordosis/2
  
  upper_arc_lordosis <- l1_s1_lordosis - l4_s1_lordosis
  
  if(inflection == "L2"){
    l3_segment_angle <- upper_arc_lordosis
    l2_segment_angle <- 0
    l1_segment_angle <- 0
    t12_segment_angle <- if_else(tk_lower <= 0, tk, add_thoracic_lordosis)
    t11_segment_angle <- if_else(tk_lower <= 1 & tk_upper >= 1, tk, add_thoracic_lordosis)
  }
  
  if(inflection == "L1"){
    l3_segment_angle <- upper_arc_lordosis/2
    l2_segment_angle <- upper_arc_lordosis/2
    l1_segment_angle <- 0
    t12_segment_angle <- if_else(tk_lower <= 0, tk, add_thoracic_lordosis)
    t11_segment_angle <- if_else(tk_lower <= 1 & tk_upper >= 1, tk, add_thoracic_lordosis)
  }
  
  if(inflection == "T12"){
    l3_segment_angle <- upper_arc_lordosis/3
    l2_segment_angle <- upper_arc_lordosis/3
    l1_segment_angle <- upper_arc_lordosis/3
    t12_segment_angle <- 0
    t11_segment_angle <- if_else(tk_lower <= 1 & tk_upper >= 1, tk, add_thoracic_lordosis)
  }
  
  if(inflection == "T11"){
    l3_segment_angle <- upper_arc_lordosis/4
    l2_segment_angle <- upper_arc_lordosis/4
    l1_segment_angle <- upper_arc_lordosis/4
    t12_segment_angle <- upper_arc_lordosis/4
    t11_segment_angle <- 0
  }
  
  
  # t12_segment_angle <- if_else(tk_lower <= 0, tk, add_thoracic_lordosis)
  # t11_segment_angle <- if_else(tk_lower <= 1 & tk_upper >= 1, tk, add_thoracic_lordosis)
  t10_segment_angle <- if_else(tk_lower <= 2 & tk_upper >= 2, tk, 0) #9 on uiv
  t9_segment_angle <-  if_else(tk_lower <= 3 & tk_upper >= 3, tk, 0) #10 on uiv
  t8_segment_angle <-  if_else(tk_lower <= 4 & tk_upper >= 4, tk, 0)
  t7_segment_angle <-  if_else(tk_lower <= 5 & tk_upper >= 5, tk, 0)
  t6_segment_angle <-  if_else(tk_lower <= 6 & tk_upper >= 6, tk, 0)
  t5_segment_angle <-  if_else(tk_lower <= 7 & tk_upper >= 7, tk, 0)
  t4_segment_angle <-  if_else(tk_lower <= 8 & tk_upper >= 8, tk, 0)
  t3_segment_angle <-  if_else(tk_lower <= 9 & tk_upper >= 9, tk, 0)
  t2_segment_angle <-  if_else(tk_lower <= 10 & tk_upper >= 10, tk, 0)
  t1_segment_angle <-  if_else(tk_lower <= 11 & tk_upper >= 11, tk, 0)
  # }
  
  segment_angle_list <-
    list(
      l5_segment_angle = l5_segment_angle,
      l4_segment_angle = l4_segment_angle,
      l3_segment_angle = l3_segment_angle,
      l2_segment_angle = l2_segment_angle,
      l1_segment_angle = l1_segment_angle,
      t12_segment_angle = t12_segment_angle,
      t11_segment_angle = t11_segment_angle,
      t10_segment_angle = t10_segment_angle,
      t9_segment_angle = t9_segment_angle,
      t8_segment_angle = t8_segment_angle,
      t7_segment_angle = t7_segment_angle,
      t6_segment_angle = t6_segment_angle,
      t5_segment_angle = t5_segment_angle,
      t4_segment_angle = t4_segment_angle,
      t3_segment_angle = t3_segment_angle,
      t2_segment_angle = t2_segment_angle,
      t1_segment_angle = t1_segment_angle
      # lumbar_segment_angles_all_df = lumbar_all_combinations_df,
      # lumbar_segment_angles_df = lumbar_segment_angles_df
    )
  
  return(segment_angle_list = segment_angle_list)
  
}


segment_angle_function_using_lumbar_segment_angles <- function(pelvic_incidence,
                                                               defined_lumbar_segment_angles_list,
                                                               spinal_kyphosis = 45,
                                                               tk_upper = 10, 
                                                               tk_lower = 1, 
                                                               add_tk = 0
){
  
  tk_range <- tk_upper-tk_lower
  
  tk <- -1 * spinal_kyphosis / tk_range
  
  thoracic_lordosis_range <- tk_lower  ## this will add lordosis evenly under wherever thoracic kyphosis starts
  
  if (thoracic_lordosis_range > 0) {
    add_thoracic_lordosis <- add_tk / tk_range
  } else {
    add_thoracic_lordosis <- 0
  }
  
  pelvic_incidence <- round(pelvic_incidence, 0)
  l1_s1_lordosis <- defined_lumbar_segment_angles_list$l1_segment_angle + 
    defined_lumbar_segment_angles_list$l2_segment_angle + 
    defined_lumbar_segment_angles_list$l3_segment_angle + 
    defined_lumbar_segment_angles_list$l4_segment_angle + 
    defined_lumbar_segment_angles_list$l5_segment_angle
  
  l5_segment_angle <- defined_lumbar_segment_angles_list$l5_segment_angle
  l4_segment_angle <- defined_lumbar_segment_angles_list$l4_segment_angle
  l3_segment_angle <- defined_lumbar_segment_angles_list$l3_segment_angle
  l2_segment_angle <- defined_lumbar_segment_angles_list$l2_segment_angle
  l1_segment_angle <- defined_lumbar_segment_angles_list$l1_segment_angle
  t12_segment_angle <- if_else(tk_lower == 0 & tk_upper >= 1, tk, add_thoracic_lordosis)
  t11_segment_angle <- if_else(tk_lower <= 1 & tk_upper >= 1, tk, add_thoracic_lordosis)
  
  t10_segment_angle <- if_else(tk_lower <= 2 & tk_upper >= 2, tk, 0) #9 on uiv
  t9_segment_angle <-  if_else(tk_lower <= 3 & tk_upper >= 3, tk, 0) #10 on uiv
  t8_segment_angle <-  if_else(tk_lower <= 4 & tk_upper >= 4, tk, 0)
  t7_segment_angle <-  if_else(tk_lower <= 5 & tk_upper >= 5, tk, 0)
  t6_segment_angle <-  if_else(tk_lower <= 6 & tk_upper >= 6, tk, 0)
  t5_segment_angle <-  if_else(tk_lower <= 7 & tk_upper >= 7, tk, 0)
  t4_segment_angle <-  if_else(tk_lower <= 8 & tk_upper >= 8, tk, 0)
  t3_segment_angle <-  if_else(tk_lower <= 9 & tk_upper >= 9, tk, 0)
  t2_segment_angle <-  if_else(tk_lower <= 10 & tk_upper >= 10, tk, 0)
  t1_segment_angle <-  if_else(tk_lower <= 11 & tk_upper >= 11, tk, 0)
  # }
  
  segment_angle_list <-
    list(
      l5_segment_angle = l5_segment_angle,
      l4_segment_angle = l4_segment_angle,
      l3_segment_angle = l3_segment_angle,
      l2_segment_angle = l2_segment_angle,
      l1_segment_angle = l1_segment_angle,
      t12_segment_angle = t12_segment_angle,
      t11_segment_angle = t11_segment_angle,
      t10_segment_angle = t10_segment_angle,
      t9_segment_angle = t9_segment_angle,
      t8_segment_angle = t8_segment_angle,
      t7_segment_angle = t7_segment_angle,
      t6_segment_angle = t6_segment_angle,
      t5_segment_angle = t5_segment_angle,
      t4_segment_angle = t4_segment_angle,
      t3_segment_angle = t3_segment_angle,
      t2_segment_angle = t2_segment_angle,
      t1_segment_angle = t1_segment_angle
      # lumbar_segment_angles_all_df = lumbar_all_combinations_df,
      # lumbar_segment_angles_df = lumbar_segment_angles_df
    )
  
  return(segment_angle_list = segment_angle_list)
  
}

