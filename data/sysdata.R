load("~/Project/CUInetwork/v20230503/sysdata.rda")
steps <- read_tsv("~/Project/CUInetwork/v20230503/doc/steps.tsv")

ColorsNet = data.frame('group' = c("Disease_Codified", "Drug_Codified", "Lab_Codified", "Procedure_Codified",
                                   "ACTI", "LIVB",  "PHEN", "PHYS", "PROC", "CHEM", 
                                   "Disease_NLP", "Drug_NLP", "Lab_NLP", "Procedure_NLP"),
                       'color.background' = 
                         c('#00C6F2', '#C7A8F0', '#30E3A4', '#FFB5D3',
                           '#D5E4ED', '#D5E4ED', '#D5E4ED', '#441DF0', '#F20C51','#A822A1',
                           '#2DAACC', '#AB22A1', '#187A1A', '#F20C51'))

ColorsCirc$group = str_replace(ColorsCirc$group, '^CCS$', 'ProcedureCode')
ColorsCirc$color = ColorsNet$color.background[match(ColorsCirc$group, ColorsNet$group)]
ColorsCirc$color[ColorsCirc$group == 'PheCode'] = 
  ColorsNet$color.background[ColorsNet$group == 'Disease_Codified']
ColorsCirc$color[ColorsCirc$group == 'RXNORM'] = 
  ColorsNet$color.background[ColorsNet$group == 'Drug_Codified']
ColorsCirc$color[ColorsCirc$group == 'LOINC'] = 
  ColorsNet$color.background[ColorsNet$group == 'Lab_Codified']
ColorsCirc$color[ColorsCirc$group == 'ProcedureCode'] = 
  ColorsNet$color.background[ColorsNet$group == 'Procedure_Codified']
ColorsCirc$color[ColorsCirc$group == 'DISO'] = 
  ColorsNet$color.background[ColorsNet$group == 'Disease_NLP']
ColorsCirc$color[ColorsCirc$group == 'Drug'] = 
  ColorsNet$color.background[ColorsNet$group == 'Drug_NLP']
ColorsCirc$color[ColorsCirc$group == 'Lab'] = 
  ColorsNet$color.background[ColorsNet$group == 'Lab_Codified']

ColorsCirc$color = sapply(ColorsCirc$color, function(x){
  if(str_detect(x, 'rgba')){
    junk = as.numeric(str_split(str_remove_all(x, 'rgba\\(|\\)'), ',')[[1]][1:3])
    rgb(junk[1], junk[2], junk[3],  maxColorValue = 255)
  }else{
    x
  }
})

shapes <- c("dot", "triangle", "square", "star", "box", "circle",
            "ellipse", "database", "text", "diamond")

colors<-c("#66C2A5","#F8C998","#FFD700","#EE82EE","#98F898","#8DA0CB","#A898F8","#82B9FD",
          "#FBD51C","#D700FE","#00FDD7","#FD0D2E","#FC844B","#E9DBF8","#0DA7FB","#85FA00",
          "#FF0DC1","#C7ED9B","#D2A27F","#FF96EB","#D82668","#45E3FD","#0D9600","#7AB9A8",
          "#B9ADFC","#FD8C9D","#63495A","#D2EF22","#AD70FB","#26FD8D","#B800B9","#8D880D",
          "#0047BB","#972A16","#D292AC","#006581","#85224D","#32511C","#6D2A95","#352EFE")


shapes_icons <- c("fa-circle",  
  "fa-mountain",
  "fa-square",
  "fa-star", 
  "fa-square-plus", 
  "fa-circle-o", 
  "fa-circle-question", 
  "fa-database", 
  "fa-font", 
  "fa-diamond")

save(attrs,colors,ColorsCirc,ColorsNet,phecode,steps,shapes,shapes_icons,
     file = "~/Project/CUInetwork/v20230503/sysdata.rda")
