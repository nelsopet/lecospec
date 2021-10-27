#
Quad_Pct<-function(output,num)
{output[[num]]
as.data.frame() %>% 
  rename(PFT = ".") %>% 
  mutate(Pix_cnt = n()) %>% 
  group_by(PFT) %>% 
  mutate(PFT_pct = n()/Pix_cnt) %>% 
  unique()}
