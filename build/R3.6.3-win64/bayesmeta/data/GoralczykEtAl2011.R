#
#  A.D.Goralczyk, N.Hauke, N.Bari, T.Y.Tsui, T.Lorf, A.Obed.
#  Interleukin-2 receptor antagonists for pediatric liver transplant recipients:
#  A systematic review and meta-analysis of controlled studies.
#  Hepatology, 54(2):541-554, 2011.
#

GoralczykEtAl2011 <- data.frame("publication"=c("Fasola (2005)", "Innocenti (2003)", "Lu (2006)",
                                                "Neuhaus (2002)", "Schmeding (2007)", "Calmus (2010)",
                                                "Heffron (2001)", "Lin (2005)", "Neuberger (2009)",
                                                "Yan (2004)", "Yoshida (2005)", "Boillot (2005)",
                                                "de Simone (2007)", "Humar (2007)", 
                                                "Kato, cohort 1 (2007)", "Kato, cohort 2 (2007)",
                                                "Klintmalm (2007)", "Lupo (2008)", "Washburn (2001)"),
                                "year"=c(2005, 2003, 2006, 2002, 2007, 2010, 2001, 2005, 2009, 2004,
                                         2005, 2005, 2007, 2007, 2007, 2007, 2007, 2008, 2001),
                                "randomized"=factor(c("yes","no","n.s.")[c(1,2,3,1,1,1,3,2,1,1,1,1,1,2,1,1,1,1,1)],
                                                    levels=c("yes","no","n.s.")),
                                "control.type"=factor(c("concurrent","historical")[c(1,1,1,1,1, 1,1,1,1,1, 1,1,1,2,1, 1,1,1,1)],
                                                  levels=c("concurrent","historical")),
                                "comparison"=factor(c("IL-2RA only","delayed CNI","no/low steroids")[c(1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3)],
                                                    levels=c("IL-2RA only","delayed CNI","no/low steroids")),
                                "IL2RA"=factor(c("basiliximab","daclizumab")[c(2,2,2,1,1,2,2,1,2,1,2,2,1,1,2,2,2,1,2)]),
                                "CNI"=factor(c("cyclosporine A","tacrolimus")[c(2,1,2,1,2,2,2,2,2,1,2,2,1,2,2,2,2,1,2)]),
                                "MMF"=factor(c("yes","no")[c(1,1,1,2,2,1,1,1,1,1,1,2,1,1,2,1,1,2,1)],
                                             levels=c("yes","no")),
                                "followup"=c(12,NA,6,12,NA,24,12,6,12,3,12,3,12,16,12,12,12,22,18),
                                "exp.AR.events"=c(13,7,3,74,29,23,14,3,28,3,17,89,17,9,7,3,80,4,1),
                                "exp.SRR.events"=c(NA,NA,NA,34,6,1,NA,NA,NA,0,NA,10,NA,0,NA,NA,NA,NA,NA),
                                "exp.deaths"=c(4,0,3,25,4,9,4,0,11,NA,10,25,7,10,NA,NA,11,4,0),
                                "exp.total"=c(46,24,40,188,51,98,54,27,168,24,72,351,95,83,15,16,153,26,15),
                                "cont.AR.events"=c(11,4,3,88,25,24,23,5,45,9,21,92,21,10,9,8,46,6,1),
                                "cont.SRR.events"=c(NA,NA,NA,51,3,3,NA,NA,NA,1,NA,22,NA,0,NA,NA,NA,NA,NA),
                                "cont.deaths"=c(2,1,2,31,3,6,4,0,19,NA,5,20,8,16,NA,NA,8,8,1),
                                "cont.total"=c(24,10,27,193,48,101,47,18,168,24,76,347,95,83,16,23,79,21,15),
                                stringsAsFactors=FALSE)[c(7,19,4,2,10,12,1,8,11,3,13,14,15,16,17,5,18,9,6),]

rownames(GoralczykEtAl2011) <- as.character(1:19)
