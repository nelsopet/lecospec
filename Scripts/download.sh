#!/bin/bash

AUTHTOKEN=[ya29.a0ARrdaM8jnY2YH1r-Fp774P-Fk_xXH52SliC3liPvoQqV139GmabvSRhrsf-ZuWvSjEfvlw-jmcYeczuGpa1kiNhgMsUNsekDttfrnQjCYM29t_BUSDcFRl-MvDw6coc1zAKbtYJjwdWeaysnVh0y-yQ6DQXi
outputFolder=/home/kbundy/lsData/alaska/

# download MurphyDome file from Google Drive automatically
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1UEo19KGOexa7G3Xk0XfR2IM2R8xAibzT?alt=media -o $outputFolder/MurphyDome
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1Ug6f-mp_FjNOsSsRG3ByOHKzTWfOQYiX?alt=media -o $outputFolder/MurphyDome.hdr
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1XzqE2vZzyyJiW3ewldOWqOp1Ew2GqI_3?alt=media -o $outputFolder/LittleLake
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/19T69RyImNSNjmHl6n6P0O7tmr6fLX63R?alt=media -o $outputFolder/LittleLake.hdr
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1uc4_T4ThjR0QzFESbCLP-WazGmRUV_Am?alt=media -o $outputFolder/WiskershamDome
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/16BEHLtM4KzqYw5ZwouraQ_LOMe1ol7eR?alt=media -o $outputFolder/WiskershamDome.hdr
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1IoBlQm2PUkUH9aEIBkOGquC5P-6Dqg9L?alt=media -o $outputFolder/TwelveMile
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1SnlbR6VuZU36_LOCHFoEHCo6TiMSiepr?alt=media -o $outputFolder/TwelveMile.hdr
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1p33XYKJGfvTJTny85uZ_yJaG7x1LCw05?alt=media -o $outputFolder/MaskedLittleLake
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1yMHlrYeSAmza94BKAEM0Jd65h5M_q4AV?alt=media -o $outputFolder/MaskedLittleLake.hdr
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1yQfMWkJn_ELvgb04K-Z8dXolExICMwAT?alt=media -o $outputFolder/EagleSummit
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1pCv9WoKs_SexdoLvRr6dx9gJJKDNJyuP?alt=media -o $outputFolder/EagleSummit.hdr
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1G4qkD_FkVGjua2G3UebatD63YNasZK3M?alt=media -o $outputFolder/EightMile
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1QMAFg99qL3B4fGtljkBTDEClr3W2KBPr?alt=media -o $outputFolder/EightMile.hdr
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1hiTtPB5cH9N25XdF9GEMUF7rOyREFP3T?alt=media -o $outputFolder/Chatnika
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1asZrrTI3UIMR46FBKZDUBG0Z6P8AoBEb?alt=media -o $outputFolder/Chatnika.hdr
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/16MiXJazZycvz5USwuAQM8Y3UO7Qq6kBz?alt=media -o $outputFolder/BisonGulch
curl -H "Authorization: Bearer $AUTHTOKEN" https://www.googleapis.com/drive/v3/files/1t2ovPqOVVeEVk42W-Cu5VfRlxu9kPRkt?alt=media -o $outputFolder/BisonGulch.hdr