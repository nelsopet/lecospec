install.packages("link2GI")
require(link2GI)


Speclibreduced_Scan_Count<-SpecLib_reduced_df %>% group_by(Class2, Class3) %>% tally() %>% rename(Reduced_Scan_Count = n)
  dim(Speclibreduced_Scan_Count)
Speclibresamp_Scan_Count<-Speclib_resamp %>% group_by(Class2, Class3) %>% tally() %>% rename(Resamp_Scan_Count = n)
  dim(Speclibresamp_Scan_Count)
Speclibnew_Scan_Count<-SpecLib_new %>% group_by(Class2, Class3) %>% tally() %>% rename(New_Scan_Count = n)
  dim(Speclibnew_Scan_Count)
Speclib_Scan_Count<-SpecLib %>% group_by(Class2, Class3) %>% tally() %>% rename(Full_Scan_Count = n)
  dim(Speclib_Scan_Count)

  inner_join(Speclib_Scan_Count,Speclibresamp_Scan_Count, by=c("Class2"="Class2","Class3"="Class3")) %>% 
    mutate(Scan_Reduction = Resamp_Scan_Count-Full_Scan_Count) %>% View()
  