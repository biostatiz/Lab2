Merge
================
Taehoon Ha
09/23/2018

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

### 4. Download two files:

-   **coverage.csv** - Health Insurance Coverage of the Total Population (2013 - 2016)
-   **expenditures.csv** - Health Care Expenditures by State of Residence (in millions)

``` r
coverage <- read.csv('coverage.csv', skip = 2, nrows = 52, header = T, sep = ',', stringsAsFactors = F)
expenditures <- read.csv('expenditures.csv', skip = 2, nrows = 52, header = T, sep = ',', stringsAsFactors = F)
```

4.1 Make the data frames "tidy."

``` r
tidy.coverage <- coverage %>% 
  gather(key = XYear__Category, value = Cases, indexes = 2:ncol(coverage)) %>%
  separate(XYear__Category, into = c("XYear", "Category"), sep = '__') %>%
  mutate(Year = gsub("X","", XYear)) %>%
  select(-XYear)

tidy.expenditures <- expenditures %>% 
  gather(XYear__Category, value = Total.Amount, indexes = 2:ncol(expenditures)) %>%
  separate(XYear__Category, into = c("XYear", "Category"), sep = '__') %>%
  mutate(Year = gsub("X","", XYear)) %>%
  select(-XYear)

tidy.coverage
```

    ##                  Location     Category     Cases Year
    ## 1           United States     Employer 155696900 2013
    ## 2                 Alabama     Employer   2126500 2013
    ## 3                  Alaska     Employer    364900 2013
    ## 4                 Arizona     Employer   2883800 2013
    ## 5                Arkansas     Employer   1128800 2013
    ## 6              California     Employer  17747300 2013
    ## 7                Colorado     Employer   2852500 2013
    ## 8             Connecticut     Employer   2030500 2013
    ## 9                Delaware     Employer    473700 2013
    ## 10   District of Columbia     Employer    324300 2013
    ## 11                Florida     Employer   8023400 2013
    ## 12                Georgia     Employer   4700500 2013
    ## 13                 Hawaii     Employer    732600 2013
    ## 14                  Idaho     Employer    802200 2013
    ## 15               Illinois     Employer   6768200 2013
    ## 16                Indiana     Employer   3494600 2013
    ## 17                   Iowa     Employer   1743100 2013
    ## 18                 Kansas     Employer   1530500 2013
    ## 19               Kentucky     Employer   2076800 2013
    ## 20              Louisiana     Employer   2083400 2013
    ## 21                  Maine     Employer    658200 2013
    ## 22               Maryland     Employer   3172400 2013
    ## 23          Massachusetts     Employer   3908800 2013
    ## 24               Michigan     Employer   5388000 2013
    ## 25              Minnesota     Employer   3234700 2013
    ## 26            Mississippi     Employer   1270000 2013
    ## 27               Missouri     Employer   3026400 2013
    ## 28                Montana     Employer    440700 2013
    ## 29               Nebraska     Employer   1050800 2013
    ## 30                 Nevada     Employer   1345500 2013
    ## 31          New Hampshire     Employer    778400 2013
    ## 32             New Jersey     Employer   5060100 2013
    ## 33             New Mexico     Employer    797400 2013
    ## 34               New York     Employer   9616000 2013
    ## 35         North Carolina     Employer   4295500 2013
    ## 36           North Dakota     Employer    413100 2013
    ## 37                   Ohio     Employer   5883900 2013
    ## 38               Oklahoma     Employer   1679100 2013
    ## 39                 Oregon     Employer   1915000 2013
    ## 40           Pennsylvania     Employer   7027800 2013
    ## 41           Rhode Island     Employer    564100 2013
    ## 42         South Carolina     Employer   2136200 2013
    ## 43           South Dakota     Employer    444100 2013
    ## 44              Tennessee     Employer   2895200 2013
    ## 45                  Texas     Employer  12283600 2013
    ## 46                   Utah     Employer   1702000 2013
    ## 47                Vermont     Employer    317700 2013
    ## 48               Virginia     Employer   4661600 2013
    ## 49             Washington     Employer   3541600 2013
    ## 50          West Virginia     Employer    841300 2013
    ## 51              Wisconsin     Employer   3154500 2013
    ## 52                Wyoming     Employer    305900 2013
    ## 53          United States    Non.Group  13816000 2013
    ## 54                Alabama    Non.Group    174200 2013
    ## 55                 Alaska    Non.Group     24000 2013
    ## 56                Arizona    Non.Group    170800 2013
    ## 57               Arkansas    Non.Group    155600 2013
    ## 58             California    Non.Group   1986400 2013
    ## 59               Colorado    Non.Group    426300 2013
    ## 60            Connecticut    Non.Group    126800 2013
    ## 61               Delaware    Non.Group     25100 2013
    ## 62   District of Columbia    Non.Group     30400 2013
    ## 63                Florida    Non.Group    968200 2013
    ## 64                Georgia    Non.Group    401600 2013
    ## 65                 Hawaii    Non.Group     47900 2013
    ## 66                  Idaho    Non.Group    114100 2013
    ## 67               Illinois    Non.Group    636900 2013
    ## 68                Indiana    Non.Group    234300 2013
    ## 69                   Iowa    Non.Group    168800 2013
    ## 70                 Kansas    Non.Group    127900 2013
    ## 71               Kentucky    Non.Group    110100 2013
    ## 72              Louisiana    Non.Group    164700 2013
    ## 73                  Maine    Non.Group     41300 2013
    ## 74               Maryland    Non.Group    320800 2013
    ## 75          Massachusetts    Non.Group    289200 2013
    ## 76               Michigan    Non.Group    317800 2013
    ## 77              Minnesota    Non.Group    305700 2013
    ## 78            Mississippi    Non.Group     88600 2013
    ## 79               Missouri    Non.Group    323100 2013
    ## 80                Montana    Non.Group     53000 2013
    ## 81               Nebraska    Non.Group    136600 2013
    ## 82                 Nevada    Non.Group    115000 2013
    ## 83          New Hampshire    Non.Group     45500 2013
    ## 84             New Jersey    Non.Group    293600 2013
    ## 85             New Mexico    Non.Group     82400 2013
    ## 86               New York    Non.Group    790500 2013
    ## 87         North Carolina    Non.Group    529800 2013
    ## 88           North Dakota    Non.Group     60700 2013
    ## 89                   Ohio    Non.Group    396500 2013
    ## 90               Oklahoma    Non.Group    133600 2013
    ## 91                 Oregon    Non.Group    203700 2013
    ## 92           Pennsylvania    Non.Group    516100 2013
    ## 93           Rhode Island    Non.Group     63700 2013
    ## 94         South Carolina    Non.Group    132300 2013
    ## 95           South Dakota    Non.Group     64000 2013
    ## 96              Tennessee    Non.Group    218200 2013
    ## 97                  Texas    Non.Group   1036800 2013
    ## 98                   Utah    Non.Group    176200 2013
    ## 99                Vermont    Non.Group     26200 2013
    ## 100              Virginia    Non.Group    364800 2013
    ## 101            Washington    Non.Group    309000 2013
    ## 102         West Virginia    Non.Group     42600 2013
    ## 103             Wisconsin    Non.Group    225300 2013
    ## 104               Wyoming    Non.Group     19500 2013
    ## 105         United States     Medicaid  54919100 2013
    ## 106               Alabama     Medicaid    869700 2013
    ## 107                Alaska     Medicaid     95000 2013
    ## 108               Arizona     Medicaid   1346100 2013
    ## 109              Arkansas     Medicaid    600800 2013
    ## 110            California     Medicaid   8344800 2013
    ## 111              Colorado     Medicaid    697300 2013
    ## 112           Connecticut     Medicaid    532000 2013
    ## 113              Delaware     Medicaid    192700 2013
    ## 114  District of Columbia     Medicaid    174900 2013
    ## 115               Florida     Medicaid   3190900 2013
    ## 116               Georgia     Medicaid   1503000 2013
    ## 117                Hawaii     Medicaid    232600 2013
    ## 118                 Idaho     Medicaid    233200 2013
    ## 119              Illinois     Medicaid   2379100 2013
    ## 120               Indiana     Medicaid   1006200 2013
    ## 121                  Iowa     Medicaid    464000 2013
    ## 122                Kansas     Medicaid    414600 2013
    ## 123              Kentucky     Medicaid    868400 2013
    ## 124             Louisiana     Medicaid    900200 2013
    ## 125                 Maine     Medicaid    277200 2013
    ## 126              Maryland     Medicaid    889800 2013
    ## 127         Massachusetts     Medicaid   1356100 2013
    ## 128              Michigan     Medicaid   1692500 2013
    ## 129             Minnesota     Medicaid    730900 2013
    ## 130           Mississippi     Medicaid    646200 2013
    ## 131              Missouri     Medicaid    863800 2013
    ## 132               Montana     Medicaid    135700 2013
    ## 133              Nebraska     Medicaid    226800 2013
    ## 134                Nevada     Medicaid    357900 2013
    ## 135         New Hampshire     Medicaid    142300 2013
    ## 136            New Jersey     Medicaid   1262900 2013
    ## 137            New Mexico     Medicaid    470000 2013
    ## 138              New York     Medicaid   4553100 2013
    ## 139        North Carolina     Medicaid   1717400 2013
    ## 140          North Dakota     Medicaid     69200 2013
    ## 141                  Ohio     Medicaid   1857000 2013
    ## 142              Oklahoma     Medicaid    699600 2013
    ## 143                Oregon     Medicaid    686300 2013
    ## 144          Pennsylvania     Medicaid   1968000 2013
    ## 145          Rhode Island     Medicaid    166100 2013
    ## 146        South Carolina     Medicaid    814900 2013
    ## 147          South Dakota     Medicaid     98400 2013
    ## 148             Tennessee     Medicaid   1244700 2013
    ## 149                 Texas     Medicaid   4312300 2013
    ## 150                  Utah     Medicaid    346600 2013
    ## 151               Vermont     Medicaid    123400 2013
    ## 152              Virginia     Medicaid    773200 2013
    ## 153            Washington     Medicaid   1026800 2013
    ## 154         West Virginia     Medicaid    382500 2013
    ## 155             Wisconsin     Medicaid    907600 2013
    ## 156               Wyoming     Medicaid     74200 2013
    ## 157         United States     Medicare  40876300 2013
    ## 158               Alabama     Medicare    783000 2013
    ## 159                Alaska     Medicare     55200 2013
    ## 160               Arizona     Medicare    842000 2013
    ## 161              Arkansas     Medicare    515200 2013
    ## 162            California     Medicare   3828500 2013
    ## 163              Colorado     Medicare    549700 2013
    ## 164           Connecticut     Medicare    475300 2013
    ## 165              Delaware     Medicare    141300 2013
    ## 166  District of Columbia     Medicare     59900 2013
    ## 167               Florida     Medicare   3108800 2013
    ## 168               Georgia     Medicare   1280400 2013
    ## 169                Hawaii     Medicare    195000 2013
    ## 170                 Idaho     Medicare    194600 2013
    ## 171              Illinois     Medicare   1539500 2013
    ## 172               Indiana     Medicare    830300 2013
    ## 173                  Iowa     Medicare    404100 2013
    ## 174                Kansas     Medicare    364900 2013
    ## 175              Kentucky     Medicare    604700 2013
    ## 176             Louisiana     Medicare    643900 2013
    ## 177                 Maine     Medicare    195100 2013
    ## 178              Maryland     Medicare    751500 2013
    ## 179         Massachusetts     Medicare    840800 2013
    ## 180              Michigan     Medicare   1384300 2013
    ## 181             Minnesota     Medicare    690700 2013
    ## 182           Mississippi     Medicare    421700 2013
    ## 183              Missouri     Medicare   1030300 2013
    ## 184               Montana     Medicare    167600 2013
    ## 185              Nebraska     Medicare    241100 2013
    ## 186                Nevada     Medicare    333500 2013
    ## 187         New Hampshire     Medicare    186100 2013
    ## 188            New Jersey     Medicare   1112600 2013
    ## 189            New Mexico     Medicare    334500 2013
    ## 190              New York     Medicare   2461600 2013
    ## 191        North Carolina     Medicare   1352600 2013
    ## 192          North Dakota     Medicare     80900 2013
    ## 193                  Ohio     Medicare   1745600 2013
    ## 194              Oklahoma     Medicare    516000 2013
    ## 195                Oregon     Medicare    597200 2013
    ## 196          Pennsylvania     Medicare   1910800 2013
    ## 197          Rhode Island     Medicare    143100 2013
    ## 198        South Carolina     Medicare    757400 2013
    ## 199          South Dakota     Medicare    120700 2013
    ## 200             Tennessee     Medicare    984400 2013
    ## 201                 Texas     Medicare   2679300 2013
    ## 202                  Utah     Medicare    269400 2013
    ## 203               Vermont     Medicare     96600 2013
    ## 204              Virginia     Medicare    968000 2013
    ## 205            Washington     Medicare    879000 2013
    ## 206         West Virginia     Medicare    329400 2013
    ## 207             Wisconsin     Medicare    812900 2013
    ## 208               Wyoming     Medicare     65400 2013
    ## 209         United States Other.Public   6295400 2013
    ## 210               Alabama Other.Public     85600 2013
    ## 211                Alaska Other.Public     60600 2013
    ## 212               Arizona Other.Public       N/A 2013
    ## 213              Arkansas Other.Public     67600 2013
    ## 214            California Other.Public    675400 2013
    ## 215              Colorado Other.Public    118100 2013
    ## 216           Connecticut Other.Public     48200 2013
    ## 217              Delaware Other.Public     13800 2013
    ## 218  District of Columbia Other.Public       N/A 2013
    ## 219               Florida Other.Public    517800 2013
    ## 220               Georgia Other.Public    334700 2013
    ## 221                Hawaii Other.Public     78600 2013
    ## 222                 Idaho Other.Public     21500 2013
    ## 223              Illinois Other.Public    110200 2013
    ## 224               Indiana Other.Public     91700 2013
    ## 225                  Iowa Other.Public     46600 2013
    ## 226                Kansas Other.Public       N/A 2013
    ## 227              Kentucky Other.Public    116000 2013
    ## 228             Louisiana Other.Public     95100 2013
    ## 229                 Maine Other.Public     21200 2013
    ## 230              Maryland Other.Public    124400 2013
    ## 231         Massachusetts Other.Public       N/A 2013
    ## 232              Michigan Other.Public     68300 2013
    ## 233             Minnesota Other.Public     48500 2013
    ## 234           Mississippi Other.Public     62500 2013
    ## 235              Missouri Other.Public     70000 2013
    ## 236               Montana Other.Public     38100 2013
    ## 237              Nebraska Other.Public     25100 2013
    ## 238                Nevada Other.Public     80500 2013
    ## 239         New Hampshire Other.Public     15100 2013
    ## 240            New Jersey Other.Public       N/A 2013
    ## 241            New Mexico Other.Public     54800 2013
    ## 242              New York Other.Public    164700 2013
    ## 243        North Carolina Other.Public    318100 2013
    ## 244          North Dakota Other.Public       N/A 2013
    ## 245                  Ohio Other.Public    172500 2013
    ## 246              Oklahoma Other.Public       N/A 2013
    ## 247                Oregon Other.Public     74800 2013
    ## 248          Pennsylvania Other.Public     94300 2013
    ## 249          Rhode Island Other.Public     12100 2013
    ## 250        South Carolina Other.Public     90000 2013
    ## 251          South Dakota Other.Public     23200 2013
    ## 252             Tennessee Other.Public       N/A 2013
    ## 253                 Texas Other.Public    665900 2013
    ## 254                  Utah Other.Public     39500 2013
    ## 255               Vermont Other.Public      9900 2013
    ## 256              Virginia Other.Public    492000 2013
    ## 257            Washington Other.Public    301400 2013
    ## 258         West Virginia Other.Public       N/A 2013
    ## 259             Wisconsin Other.Public     41200 2013
    ## 260               Wyoming Other.Public     25800 2013
    ## 261         United States    Uninsured  41795100 2013
    ## 262               Alabama    Uninsured    724800 2013
    ## 263                Alaska    Uninsured    102200 2013
    ## 264               Arizona    Uninsured   1223000 2013
    ## 265              Arkansas    Uninsured    436800 2013
    ## 266            California    Uninsured   5594100 2013
    ## 267              Colorado    Uninsured    654000 2013
    ## 268           Connecticut    Uninsured    366100 2013
    ## 269              Delaware    Uninsured     62700 2013
    ## 270  District of Columbia    Uninsured     51100 2013
    ## 271               Florida    Uninsured   3619700 2013
    ## 272               Georgia    Uninsured   1592800 2013
    ## 273                Hawaii    Uninsured     68600 2013
    ## 274                 Idaho    Uninsured    235000 2013
    ## 275              Illinois    Uninsured   1335800 2013
    ## 276               Indiana    Uninsured    814200 2013
    ## 277                  Iowa    Uninsured    250800 2013
    ## 278                Kansas    Uninsured    283000 2013
    ## 279              Kentucky    Uninsured    624100 2013
    ## 280             Louisiana    Uninsured    645000 2013
    ## 281                 Maine    Uninsured    123600 2013
    ## 282              Maryland    Uninsured    682000 2013
    ## 283         Massachusetts    Uninsured    208700 2013
    ## 284              Michigan    Uninsured   1041500 2013
    ## 285             Minnesota    Uninsured    370800 2013
    ## 286           Mississippi    Uninsured    414600 2013
    ## 287              Missouri    Uninsured    655800 2013
    ## 288               Montana    Uninsured    158000 2013
    ## 289              Nebraska    Uninsured    171600 2013
    ## 290                Nevada    Uninsured    534700 2013
    ## 291         New Hampshire    Uninsured    151000 2013
    ## 292            New Jersey    Uninsured   1034500 2013
    ## 293            New Mexico    Uninsured    342200 2013
    ## 294              New York    Uninsured   1885100 2013
    ## 295        North Carolina    Uninsured   1439100 2013
    ## 296          North Dakota    Uninsured     77200 2013
    ## 297                  Ohio    Uninsured   1358100 2013
    ## 298              Oklahoma    Uninsured    574200 2013
    ## 299                Oregon    Uninsured    481400 2013
    ## 300          Pennsylvania    Uninsured   1255700 2013
    ## 301          Rhode Island    Uninsured     95300 2013
    ## 302        South Carolina    Uninsured    758000 2013
    ## 303          South Dakota    Uninsured     83100 2013
    ## 304             Tennessee    Uninsured    844200 2013
    ## 305                 Texas    Uninsured   5405700 2013
    ## 306                  Utah    Uninsured    359700 2013
    ## 307               Vermont    Uninsured     47700 2013
    ## 308              Virginia    Uninsured    944500 2013
    ## 309            Washington    Uninsured    808200 2013
    ## 310         West Virginia    Uninsured    213800 2013
    ## 311             Wisconsin    Uninsured    500300 2013
    ## 312               Wyoming    Uninsured     91300 2013
    ## 313         United States        Total 313401200 2013
    ## 314               Alabama        Total   4763900 2013
    ## 315                Alaska        Total    702000 2013
    ## 316               Arizona        Total   6603100 2013
    ## 317              Arkansas        Total   2904800 2013
    ## 318            California        Total  38176400 2013
    ## 319              Colorado        Total   5297800 2013
    ## 320           Connecticut        Total   3578900 2013
    ## 321              Delaware        Total    909300 2013
    ## 322  District of Columbia        Total    652100 2013
    ## 323               Florida        Total  19429000 2013
    ## 324               Georgia        Total   9813000 2013
    ## 325                Hawaii        Total   1355600 2013
    ## 326                 Idaho        Total   1600600 2013
    ## 327              Illinois        Total  12769500 2013
    ## 328               Indiana        Total   6471300 2013
    ## 329                  Iowa        Total   3077400 2013
    ## 330                Kansas        Total   2817600 2013
    ## 331              Kentucky        Total   4400100 2013
    ## 332             Louisiana        Total   4532300 2013
    ## 333                 Maine        Total   1316500 2013
    ## 334              Maryland        Total   5940900 2013
    ## 335         Massachusetts        Total   6647700 2013
    ## 336              Michigan        Total   9892400 2013
    ## 337             Minnesota        Total   5381300 2013
    ## 338           Mississippi        Total   2903600 2013
    ## 339              Missouri        Total   5969400 2013
    ## 340               Montana        Total    993200 2013
    ## 341              Nebraska        Total   1852000 2013
    ## 342                Nevada        Total   2767100 2013
    ## 343         New Hampshire        Total   1318500 2013
    ## 344            New Jersey        Total   8807400 2013
    ## 345            New Mexico        Total   2081300 2013
    ## 346              New York        Total  19471100 2013
    ## 347        North Carolina        Total   9652400 2013
    ## 348          North Dakota        Total    714500 2013
    ## 349                  Ohio        Total  11413600 2013
    ## 350              Oklahoma        Total   3709400 2013
    ## 351                Oregon        Total   3960300 2013
    ## 352          Pennsylvania        Total  12772600 2013
    ## 353          Rhode Island        Total   1044300 2013
    ## 354        South Carolina        Total   4688800 2013
    ## 355          South Dakota        Total    833400 2013
    ## 356             Tennessee        Total   6400200 2013
    ## 357                 Texas        Total  26383500 2013
    ## 358                  Utah        Total   2893500 2013
    ## 359               Vermont        Total    621400 2013
    ## 360              Virginia        Total   8204100 2013
    ## 361            Washington        Total   6866000 2013
    ## 362         West Virginia        Total   1822000 2013
    ## 363             Wisconsin        Total   5641900 2013
    ## 364               Wyoming        Total    582200 2013
    ## 365         United States     Employer 154347500 2014
    ## 366               Alabama     Employer   2202800 2014
    ## 367                Alaska     Employer    345300 2014
    ## 368               Arizona     Employer   2835200 2014
    ## 369              Arkansas     Employer   1176500 2014
    ## 370            California     Employer  17703700 2014
    ## 371              Colorado     Employer   2489400 2014
    ## 372           Connecticut     Employer   2086800 2014
    ## 373              Delaware     Employer    488100 2014
    ## 374  District of Columbia     Employer    354100 2014
    ## 375               Florida     Employer   7731600 2014
    ## 376               Georgia     Employer   4697300 2014
    ## 377                Hawaii     Employer    724800 2014
    ## 378                 Idaho     Employer    825700 2014
    ## 379              Illinois     Employer   6629700 2014
    ## 380               Indiana     Employer   3296700 2014
    ## 381                  Iowa     Employer   1643600 2014
    ## 382                Kansas     Employer   1495700 2014
    ## 383              Kentucky     Employer   1965500 2014
    ## 384             Louisiana     Employer   2067300 2014
    ## 385                 Maine     Employer    616000 2014
    ## 386              Maryland     Employer   3558800 2014
    ## 387         Massachusetts     Employer   3549900 2014
    ## 388              Michigan     Employer   5176600 2014
    ## 389             Minnesota     Employer   3119000 2014
    ## 390           Mississippi     Employer   1192400 2014
    ## 391              Missouri     Employer   3198400 2014
    ## 392               Montana     Employer    478200 2014
    ## 393              Nebraska     Employer    999000 2014
    ## 394                Nevada     Employer   1324700 2014
    ## 395         New Hampshire     Employer    784300 2014
    ## 396            New Jersey     Employer   4877600 2014
    ## 397            New Mexico     Employer    751700 2014
    ## 398              New York     Employer   9575500 2014
    ## 399        North Carolina     Employer   4689700 2014
    ## 400          North Dakota     Employer    433000 2014
    ## 401                  Ohio     Employer   5777000 2014
    ## 402              Oklahoma     Employer   1692100 2014
    ## 403                Oregon     Employer   1840400 2014
    ## 404          Pennsylvania     Employer   6652200 2014
    ## 405          Rhode Island     Employer    562100 2014
    ## 406        South Carolina     Employer   2179100 2014
    ## 407          South Dakota     Employer    463300 2014
    ## 408             Tennessee     Employer   3230900 2014
    ## 409                 Texas     Employer  12620500 2014
    ## 410                  Utah     Employer   1728900 2014
    ## 411               Vermont     Employer    321100 2014
    ## 412              Virginia     Employer   4514000 2014
    ## 413            Washington     Employer   3400100 2014
    ## 414         West Virginia     Employer    783000 2014
    ## 415             Wisconsin     Employer   3150100 2014
    ## 416               Wyoming     Employer    348200 2014
    ## 417         United States    Non.Group  19313000 2014
    ## 418               Alabama    Non.Group    288900 2014
    ## 419                Alaska    Non.Group     26800 2014
    ## 420               Arizona    Non.Group    333500 2014
    ## 421              Arkansas    Non.Group    231700 2014
    ## 422            California    Non.Group   2778800 2014
    ## 423              Colorado    Non.Group    397900 2014
    ## 424           Connecticut    Non.Group    223500 2014
    ## 425              Delaware    Non.Group     39200 2014
    ## 426  District of Columbia    Non.Group     39900 2014
    ## 427               Florida    Non.Group   1563400 2014
    ## 428               Georgia    Non.Group    551200 2014
    ## 429                Hawaii    Non.Group     44000 2014
    ## 430                 Idaho    Non.Group    126900 2014
    ## 431              Illinois    Non.Group    722000 2014
    ## 432               Indiana    Non.Group    341100 2014
    ## 433                  Iowa    Non.Group    237400 2014
    ## 434                Kansas    Non.Group    167000 2014
    ## 435              Kentucky    Non.Group    271900 2014
    ## 436             Louisiana    Non.Group    231100 2014
    ## 437                 Maine    Non.Group     67000 2014
    ## 438              Maryland    Non.Group    361700 2014
    ## 439         Massachusetts    Non.Group    352200 2014
    ## 440              Michigan    Non.Group    556400 2014
    ## 441             Minnesota    Non.Group    366600 2014
    ## 442           Mississippi    Non.Group    146500 2014
    ## 443              Missouri    Non.Group    370700 2014
    ## 444               Montana    Non.Group     85900 2014
    ## 445              Nebraska    Non.Group    128100 2014
    ## 446                Nevada    Non.Group    141100 2014
    ## 447         New Hampshire    Non.Group     74700 2014
    ## 448            New Jersey    Non.Group    381500 2014
    ## 449            New Mexico    Non.Group     94800 2014
    ## 450              New York    Non.Group   1148400 2014
    ## 451        North Carolina    Non.Group    570200 2014
    ## 452          North Dakota    Non.Group     58800 2014
    ## 453                  Ohio    Non.Group    456800 2014
    ## 454              Oklahoma    Non.Group    203900 2014
    ## 455                Oregon    Non.Group    290300 2014
    ## 456          Pennsylvania    Non.Group    793200 2014
    ## 457          Rhode Island    Non.Group     99500 2014
    ## 458        South Carolina    Non.Group    177500 2014
    ## 459          South Dakota    Non.Group     60000 2014
    ## 460             Tennessee    Non.Group    382800 2014
    ## 461                 Texas    Non.Group   1545300 2014
    ## 462                  Utah    Non.Group    223600 2014
    ## 463               Vermont    Non.Group     42900 2014
    ## 464              Virginia    Non.Group    596000 2014
    ## 465            Washington    Non.Group    472400 2014
    ## 466         West Virginia    Non.Group     68700 2014
    ## 467             Wisconsin    Non.Group    348200 2014
    ## 468               Wyoming    Non.Group     31600 2014
    ## 469         United States     Medicaid  61650400 2014
    ## 470               Alabama     Medicaid    891900 2014
    ## 471                Alaska     Medicaid    130100 2014
    ## 472               Arizona     Medicaid   1639400 2014
    ## 473              Arkansas     Medicaid    639200 2014
    ## 474            California     Medicaid   9618800 2014
    ## 475              Colorado     Medicaid   1053700 2014
    ## 476           Connecticut     Medicaid    542000 2014
    ## 477              Delaware     Medicaid    167200 2014
    ## 478  District of Columbia     Medicaid    162600 2014
    ## 479               Florida     Medicaid   3755500 2014
    ## 480               Georgia     Medicaid   1643000 2014
    ## 481                Hawaii     Medicaid    228300 2014
    ## 482                 Idaho     Medicaid    268600 2014
    ## 483              Illinois     Medicaid   2436100 2014
    ## 484               Indiana     Medicaid   1095800 2014
    ## 485                  Iowa     Medicaid    512900 2014
    ## 486                Kansas     Medicaid    430100 2014
    ## 487              Kentucky     Medicaid    987300 2014
    ## 488             Louisiana     Medicaid   1036100 2014
    ## 489                 Maine     Medicaid    255400 2014
    ## 490              Maryland     Medicaid    807900 2014
    ## 491         Massachusetts     Medicaid   1570100 2014
    ## 492              Michigan     Medicaid   2007900 2014
    ## 493             Minnesota     Medicaid    803700 2014
    ## 494           Mississippi     Medicaid    762900 2014
    ## 495              Missouri     Medicaid    879300 2014
    ## 496               Montana     Medicaid    135400 2014
    ## 497              Nebraska     Medicaid    266100 2014
    ## 498                Nevada     Medicaid    494600 2014
    ## 499         New Hampshire     Medicaid    158100 2014
    ## 500            New Jersey     Medicaid   1505400 2014
    ## 501            New Mexico     Medicaid    579800 2014
    ## 502              New York     Medicaid   4957600 2014
    ## 503        North Carolina     Medicaid   1760200 2014
    ## 504          North Dakota     Medicaid     65400 2014
    ## 505                  Ohio     Medicaid   2471700 2014
    ## 506              Oklahoma     Medicaid    648300 2014
    ## 507                Oregon     Medicaid    825400 2014
    ## 508          Pennsylvania     Medicaid   2127200 2014
    ## 509          Rhode Island     Medicaid    185300 2014
    ## 510        South Carolina     Medicaid    968100 2014
    ## 511          South Dakota     Medicaid    104100 2014
    ## 512             Tennessee     Medicaid   1065800 2014
    ## 513                 Texas     Medicaid   4725300 2014
    ## 514                  Utah     Medicaid    304600 2014
    ## 515               Vermont     Medicaid    127000 2014
    ## 516              Virginia     Medicaid    748300 2014
    ## 517            Washington     Medicaid   1560300 2014
    ## 518         West Virginia     Medicaid    528800 2014
    ## 519             Wisconsin     Medicaid    956600 2014
    ## 520               Wyoming     Medicaid     54900 2014
    ## 521         United States     Medicare  41896500 2014
    ## 522               Alabama     Medicare    718400 2014
    ## 523                Alaska     Medicare     55300 2014
    ## 524               Arizona     Medicare    911100 2014
    ## 525              Arkansas     Medicare    479400 2014
    ## 526            California     Medicare   4049000 2014
    ## 527              Colorado     Medicare    619500 2014
    ## 528           Connecticut     Medicare    435400 2014
    ## 529              Delaware     Medicare    141100 2014
    ## 530  District of Columbia     Medicare     51900 2014
    ## 531               Florida     Medicare   3324700 2014
    ## 532               Georgia     Medicare   1230400 2014
    ## 533                Hawaii     Medicare    205500 2014
    ## 534                 Idaho     Medicare    181900 2014
    ## 535              Illinois     Medicare   1754400 2014
    ## 536               Indiana     Medicare    980700 2014
    ## 537                  Iowa     Medicare    443500 2014
    ## 538                Kansas     Medicare    373100 2014
    ## 539              Kentucky     Medicare    713800 2014
    ## 540             Louisiana     Medicare    517100 2014
    ## 541                 Maine     Medicare    213500 2014
    ## 542              Maryland     Medicare    730900 2014
    ## 543         Massachusetts     Medicare    860500 2014
    ## 544              Michigan     Medicare   1414500 2014
    ## 545             Minnesota     Medicare    713500 2014
    ## 546           Mississippi     Medicare    390500 2014
    ## 547              Missouri     Medicare    894600 2014
    ## 548               Montana     Medicare    151100 2014
    ## 549              Nebraska     Medicare    258500 2014
    ## 550                Nevada     Medicare    368300 2014
    ## 551         New Hampshire     Medicare    198600 2014
    ## 552            New Jersey     Medicare   1130900 2014
    ## 553            New Mexico     Medicare    306100 2014
    ## 554              New York     Medicare   2366700 2014
    ## 555        North Carolina     Medicare   1348900 2014
    ## 556          North Dakota     Medicare     94800 2014
    ## 557                  Ohio     Medicare   1856000 2014
    ## 558              Oklahoma     Medicare    538000 2014
    ## 559                Oregon     Medicare    635700 2014
    ## 560          Pennsylvania     Medicare   1948700 2014
    ## 561          Rhode Island     Medicare    136800 2014
    ## 562        South Carolina     Medicare    732400 2014
    ## 563          South Dakota     Medicare    122800 2014
    ## 564             Tennessee     Medicare    988800 2014
    ## 565                 Texas     Medicare   2787600 2014
    ## 566                  Utah     Medicare    296300 2014
    ## 567               Vermont     Medicare     80500 2014
    ## 568              Virginia     Medicare   1070300 2014
    ## 569            Washington     Medicare    878600 2014
    ## 570         West Virginia     Medicare    302900 2014
    ## 571             Wisconsin     Medicare    827500 2014
    ## 572               Wyoming     Medicare     65600 2014
    ## 573         United States Other.Public   5985000 2014
    ## 574               Alabama Other.Public    143900 2014
    ## 575                Alaska Other.Public     37300 2014
    ## 576               Arizona Other.Public       N/A 2014
    ## 577              Arkansas Other.Public     82000 2014
    ## 578            California Other.Public    634400 2014
    ## 579              Colorado Other.Public    214000 2014
    ## 580           Connecticut Other.Public     40900 2014
    ## 581              Delaware Other.Public     30700 2014
    ## 582  District of Columbia Other.Public       N/A 2014
    ## 583               Florida Other.Public    473300 2014
    ## 584               Georgia Other.Public    296700 2014
    ## 585                Hawaii Other.Public     89900 2014
    ## 586                 Idaho Other.Public     37300 2014
    ## 587              Illinois Other.Public    114400 2014
    ## 588               Indiana Other.Public       N/A 2014
    ## 589                  Iowa Other.Public     51600 2014
    ## 590                Kansas Other.Public       N/A 2014
    ## 591              Kentucky Other.Public       N/A 2014
    ## 592             Louisiana Other.Public    117000 2014
    ## 593                 Maine Other.Public     26000 2014
    ## 594              Maryland Other.Public    136700 2014
    ## 595         Massachusetts Other.Public       N/A 2014
    ## 596              Michigan Other.Public     53100 2014
    ## 597             Minnesota Other.Public     50500 2014
    ## 598           Mississippi Other.Public    108800 2014
    ## 599              Missouri Other.Public     93300 2014
    ## 600               Montana Other.Public     31900 2014
    ## 601              Nebraska Other.Public     49200 2014
    ## 602                Nevada Other.Public    138000 2014
    ## 603         New Hampshire Other.Public       N/A 2014
    ## 604            New Jersey Other.Public     89500 2014
    ## 605            New Mexico Other.Public     66000 2014
    ## 606              New York Other.Public    110500 2014
    ## 607        North Carolina Other.Public    307200 2014
    ## 608          North Dakota Other.Public     17300 2014
    ## 609                  Ohio Other.Public    116400 2014
    ## 610              Oklahoma Other.Public     76100 2014
    ## 611                Oregon Other.Public     59700 2014
    ## 612          Pennsylvania Other.Public     85100 2014
    ## 613          Rhode Island Other.Public       N/A 2014
    ## 614        South Carolina Other.Public     93100 2014
    ## 615          South Dakota Other.Public     19000 2014
    ## 616             Tennessee Other.Public       N/A 2014
    ## 617                 Texas Other.Public    509200 2014
    ## 618                  Utah Other.Public     36400 2014
    ## 619               Vermont Other.Public      9900 2014
    ## 620              Virginia Other.Public    514300 2014
    ## 621            Washington Other.Public    126300 2014
    ## 622         West Virginia Other.Public     24000 2014
    ## 623             Wisconsin Other.Public       N/A 2014
    ## 624               Wyoming Other.Public     13600 2014
    ## 625         United States    Uninsured  32967500 2014
    ## 626               Alabama    Uninsured    522200 2014
    ## 627                Alaska    Uninsured    100800 2014
    ## 628               Arizona    Uninsured    827100 2014
    ## 629              Arkansas    Uninsured    287200 2014
    ## 630            California    Uninsured   3916700 2014
    ## 631              Colorado    Uninsured    602900 2014
    ## 632           Connecticut    Uninsured    249200 2014
    ## 633              Delaware    Uninsured     63000 2014
    ## 634  District of Columbia    Uninsured     42300 2014
    ## 635               Florida    Uninsured   2874800 2014
    ## 636               Georgia    Uninsured   1546500 2014
    ## 637                Hawaii    Uninsured     72800 2014
    ## 638                 Idaho    Uninsured    169800 2014
    ## 639              Illinois    Uninsured   1141300 2014
    ## 640               Indiana    Uninsured    687900 2014
    ## 641                  Iowa    Uninsured    191800 2014
    ## 642                Kansas    Uninsured    307400 2014
    ## 643              Kentucky    Uninsured    289300 2014
    ## 644             Louisiana    Uninsured    588000 2014
    ## 645                 Maine    Uninsured    121800 2014
    ## 646              Maryland    Uninsured    343000 2014
    ## 647         Massachusetts    Uninsured    293800 2014
    ## 648              Michigan    Uninsured    697900 2014
    ## 649             Minnesota    Uninsured    365300 2014
    ## 650           Mississippi    Uninsured    364100 2014
    ## 651              Missouri    Uninsured    525100 2014
    ## 652               Montana    Uninsured    126600 2014
    ## 653              Nebraska    Uninsured    180200 2014
    ## 654                Nevada    Uninsured    356800 2014
    ## 655         New Hampshire    Uninsured     95000 2014
    ## 656            New Jersey    Uninsured    954500 2014
    ## 657            New Mexico    Uninsured    236900 2014
    ## 658              New York    Uninsured   1520800 2014
    ## 659        North Carolina    Uninsured   1159500 2014
    ## 660          North Dakota    Uninsured     64200 2014
    ## 661                  Ohio    Uninsured    857700 2014
    ## 662              Oklahoma    Uninsured    583400 2014
    ## 663                Oregon    Uninsured    310800 2014
    ## 664          Pennsylvania    Uninsured   1020800 2014
    ## 665          Rhode Island    Uninsured     56800 2014
    ## 666        South Carolina    Uninsured    614100 2014
    ## 667          South Dakota    Uninsured     77600 2014
    ## 668             Tennessee    Uninsured    625000 2014
    ## 669                 Texas    Uninsured   4499500 2014
    ## 670                  Utah    Uninsured    339600 2014
    ## 671               Vermont    Uninsured     35600 2014
    ## 672              Virginia    Uninsured    815900 2014
    ## 673            Washington    Uninsured    647200 2014
    ## 674         West Virginia    Uninsured    118200 2014
    ## 675             Wisconsin    Uninsured    420800 2014
    ## 676               Wyoming    Uninsured     58100 2014
    ## 677         United States        Total 316159900 2014
    ## 678               Alabama        Total   4768000 2014
    ## 679                Alaska        Total    695700 2014
    ## 680               Arizona        Total   6657200 2014
    ## 681              Arkansas        Total   2896000 2014
    ## 682            California        Total  38701300 2014
    ## 683              Colorado        Total   5377400 2014
    ## 684           Connecticut        Total   3577900 2014
    ## 685              Delaware        Total    929500 2014
    ## 686  District of Columbia        Total    656900 2014
    ## 687               Florida        Total  19731100 2014
    ## 688               Georgia        Total   9965100 2014
    ## 689                Hawaii        Total   1365400 2014
    ## 690                 Idaho        Total   1610200 2014
    ## 691              Illinois        Total  12797900 2014
    ## 692               Indiana        Total   6477500 2014
    ## 693                  Iowa        Total   3080800 2014
    ## 694                Kansas        Total   2853000 2014
    ## 695              Kentucky        Total   4315700 2014
    ## 696             Louisiana        Total   4556500 2014
    ## 697                 Maine        Total   1299600 2014
    ## 698              Maryland        Total   5938900 2014
    ## 699         Massachusetts        Total   6658100 2014
    ## 700              Michigan        Total   9906400 2014
    ## 701             Minnesota        Total   5418500 2014
    ## 702           Mississippi        Total   2965300 2014
    ## 703              Missouri        Total   5961300 2014
    ## 704               Montana        Total   1009100 2014
    ## 705              Nebraska        Total   1881000 2014
    ## 706                Nevada        Total   2823400 2014
    ## 707         New Hampshire        Total   1319700 2014
    ## 708            New Jersey        Total   8939300 2014
    ## 709            New Mexico        Total   2035200 2014
    ## 710              New York        Total  19679400 2014
    ## 711        North Carolina        Total   9835800 2014
    ## 712          North Dakota        Total    733400 2014
    ## 713                  Ohio        Total  11535600 2014
    ## 714              Oklahoma        Total   3741700 2014
    ## 715                Oregon        Total   3962300 2014
    ## 716          Pennsylvania        Total  12627100 2014
    ## 717          Rhode Island        Total   1048200 2014
    ## 718        South Carolina        Total   4764300 2014
    ## 719          South Dakota        Total    846800 2014
    ## 720             Tennessee        Total   6502000 2014
    ## 721                 Texas        Total  26687400 2014
    ## 722                  Utah        Total   2929400 2014
    ## 723               Vermont        Total    617000 2014
    ## 724              Virginia        Total   8258800 2014
    ## 725            Washington        Total   7085000 2014
    ## 726         West Virginia        Total   1825500 2014
    ## 727             Wisconsin        Total   5747200 2014
    ## 728               Wyoming        Total    572000 2014
    ## 729         United States     Employer 155965800 2015
    ## 730               Alabama     Employer   2218000 2015
    ## 731                Alaska     Employer    355700 2015
    ## 732               Arizona     Employer   2766500 2015
    ## 733              Arkansas     Employer   1293700 2015
    ## 734            California     Employer  17718300 2015
    ## 735              Colorado     Employer   2706000 2015
    ## 736           Connecticut     Employer   1857800 2015
    ## 737              Delaware     Employer    505800 2015
    ## 738  District of Columbia     Employer    349900 2015
    ## 739               Florida     Employer   7879800 2015
    ## 740               Georgia     Employer   4649000 2015
    ## 741                Hawaii     Employer    720000 2015
    ## 742                 Idaho     Employer    798000 2015
    ## 743              Illinois     Employer   6842600 2015
    ## 744               Indiana     Employer   3383600 2015
    ## 745                  Iowa     Employer   1641000 2015
    ## 746                Kansas     Employer   1542700 2015
    ## 747              Kentucky     Employer   1983800 2015
    ## 748             Louisiana     Employer   2125700 2015
    ## 749                 Maine     Employer    634200 2015
    ## 750              Maryland     Employer   3431400 2015
    ## 751         Massachusetts     Employer   3696000 2015
    ## 752              Michigan     Employer   5185900 2015
    ## 753             Minnesota     Employer   3041800 2015
    ## 754           Mississippi     Employer   1207700 2015
    ## 755              Missouri     Employer   3345700 2015
    ## 756               Montana     Employer    469000 2015
    ## 757              Nebraska     Employer   1029100 2015
    ## 758                Nevada     Employer   1328200 2015
    ## 759         New Hampshire     Employer    794600 2015
    ## 760            New Jersey     Employer   4909400 2015
    ## 761            New Mexico     Employer    760100 2015
    ## 762              New York     Employer   9607700 2015
    ## 763        North Carolina     Employer   4709100 2015
    ## 764          North Dakota     Employer    436500 2015
    ## 765                  Ohio     Employer   5974700 2015
    ## 766              Oklahoma     Employer   1782600 2015
    ## 767                Oregon     Employer   1865500 2015
    ## 768          Pennsylvania     Employer   6870000 2015
    ## 769          Rhode Island     Employer    593700 2015
    ## 770        South Carolina     Employer   2201700 2015
    ## 771          South Dakota     Employer    447500 2015
    ## 772             Tennessee     Employer   2969100 2015
    ## 773                 Texas     Employer  13119300 2015
    ## 774                  Utah     Employer   1779500 2015
    ## 775               Vermont     Employer    309000 2015
    ## 776              Virginia     Employer   4332900 2015
    ## 777            Washington     Employer   3606400 2015
    ## 778         West Virginia     Employer    722800 2015
    ## 779             Wisconsin     Employer   3146300 2015
    ## 780               Wyoming     Employer    320200 2015
    ## 781         United States    Non.Group  21816500 2015
    ## 782               Alabama    Non.Group    291500 2015
    ## 783                Alaska    Non.Group     22300 2015
    ## 784               Arizona    Non.Group    278400 2015
    ## 785              Arkansas    Non.Group    200200 2015
    ## 786            California    Non.Group   3444200 2015
    ## 787              Colorado    Non.Group    346900 2015
    ## 788           Connecticut    Non.Group    302700 2015
    ## 789              Delaware    Non.Group     48600 2015
    ## 790  District of Columbia    Non.Group     53500 2015
    ## 791               Florida    Non.Group   1949500 2015
    ## 792               Georgia    Non.Group    583200 2015
    ## 793                Hawaii    Non.Group     38300 2015
    ## 794                 Idaho    Non.Group    124600 2015
    ## 795              Illinois    Non.Group    812600 2015
    ## 796               Indiana    Non.Group    317100 2015
    ## 797                  Iowa    Non.Group    249700 2015
    ## 798                Kansas    Non.Group    206900 2015
    ## 799              Kentucky    Non.Group    378100 2015
    ## 800             Louisiana    Non.Group    315100 2015
    ## 801                 Maine    Non.Group     70100 2015
    ## 802              Maryland    Non.Group    371400 2015
    ## 803         Massachusetts    Non.Group    368200 2015
    ## 804              Michigan    Non.Group    561800 2015
    ## 805             Minnesota    Non.Group    423600 2015
    ## 806           Mississippi    Non.Group    160100 2015
    ## 807              Missouri    Non.Group    398600 2015
    ## 808               Montana    Non.Group     65500 2015
    ## 809              Nebraska    Non.Group    133800 2015
    ## 810                Nevada    Non.Group    200200 2015
    ## 811         New Hampshire    Non.Group     65200 2015
    ## 812            New Jersey    Non.Group    512800 2015
    ## 813            New Mexico    Non.Group     98200 2015
    ## 814              New York    Non.Group   1440600 2015
    ## 815        North Carolina    Non.Group    710400 2015
    ## 816          North Dakota    Non.Group     61100 2015
    ## 817                  Ohio    Non.Group    603700 2015
    ## 818              Oklahoma    Non.Group    262300 2015
    ## 819                Oregon    Non.Group    272400 2015
    ## 820          Pennsylvania    Non.Group    673000 2015
    ## 821          Rhode Island    Non.Group     72100 2015
    ## 822        South Carolina    Non.Group    278100 2015
    ## 823          South Dakota    Non.Group     73200 2015
    ## 824             Tennessee    Non.Group    415400 2015
    ## 825                 Texas    Non.Group   1815300 2015
    ## 826                  Utah    Non.Group    209100 2015
    ## 827               Vermont    Non.Group     44300 2015
    ## 828              Virginia    Non.Group    638000 2015
    ## 829            Washington    Non.Group    413700 2015
    ## 830         West Virginia    Non.Group     78700 2015
    ## 831             Wisconsin    Non.Group    323000 2015
    ## 832               Wyoming    Non.Group     39300 2015
    ## 833         United States     Medicaid  62384500 2015
    ## 834               Alabama     Medicaid    911400 2015
    ## 835                Alaska     Medicaid    128100 2015
    ## 836               Arizona     Medicaid   1711500 2015
    ## 837              Arkansas     Medicaid    641400 2015
    ## 838            California     Medicaid  10138100 2015
    ## 839              Colorado     Medicaid   1036600 2015
    ## 840           Connecticut     Medicaid    690400 2015
    ## 841              Delaware     Medicaid    174700 2015
    ## 842  District of Columbia     Medicaid    175400 2015
    ## 843               Florida     Medicaid   3610400 2015
    ## 844               Georgia     Medicaid   1914400 2015
    ## 845                Hawaii     Medicaid    243800 2015
    ## 846                 Idaho     Medicaid    292700 2015
    ## 847              Illinois     Medicaid   2405700 2015
    ## 848               Indiana     Medicaid   1218500 2015
    ## 849                  Iowa     Medicaid    538700 2015
    ## 850                Kansas     Medicaid    384300 2015
    ## 851              Kentucky     Medicaid    964400 2015
    ## 852             Louisiana     Medicaid    921700 2015
    ## 853                 Maine     Medicaid    306300 2015
    ## 854              Maryland     Medicaid    856800 2015
    ## 855         Massachusetts     Medicaid   1575800 2015
    ## 856              Michigan     Medicaid   1912700 2015
    ## 857             Minnesota     Medicaid    785700 2015
    ## 858           Mississippi     Medicaid    689900 2015
    ## 859              Missouri     Medicaid    757600 2015
    ## 860               Montana     Medicaid    165600 2015
    ## 861              Nebraska     Medicaid    249600 2015
    ## 862                Nevada     Medicaid    489300 2015
    ## 863         New Hampshire     Medicaid    165300 2015
    ## 864            New Jersey     Medicaid   1614300 2015
    ## 865            New Mexico     Medicaid    551000 2015
    ## 866              New York     Medicaid   4658100 2015
    ## 867        North Carolina     Medicaid   1787300 2015
    ## 868          North Dakota     Medicaid     79700 2015
    ## 869                  Ohio     Medicaid   2383300 2015
    ## 870              Oklahoma     Medicaid    671500 2015
    ## 871                Oregon     Medicaid    964200 2015
    ## 872          Pennsylvania     Medicaid   2256200 2015
    ## 873          Rhode Island     Medicaid    176500 2015
    ## 874        South Carolina     Medicaid    897900 2015
    ## 875          South Dakota     Medicaid    117000 2015
    ## 876             Tennessee     Medicaid   1249600 2015
    ## 877                 Texas     Medicaid   4380400 2015
    ## 878                  Utah     Medicaid    357200 2015
    ## 879               Vermont     Medicaid    120500 2015
    ## 880              Virginia     Medicaid    939300 2015
    ## 881            Washington     Medicaid   1548200 2015
    ## 882         West Virginia     Medicaid    513200 2015
    ## 883             Wisconsin     Medicaid   1002800 2015
    ## 884               Wyoming     Medicaid     59900 2015
    ## 885         United States     Medicare  43308400 2015
    ## 886               Alabama     Medicare    719100 2015
    ## 887                Alaska     Medicare     60900 2015
    ## 888               Arizona     Medicare    949000 2015
    ## 889              Arkansas     Medicare    484500 2015
    ## 890            California     Medicare   4080100 2015
    ## 891              Colorado     Medicare    708000 2015
    ## 892           Connecticut     Medicare    474700 2015
    ## 893              Delaware     Medicare    138200 2015
    ## 894  District of Columbia     Medicare     66400 2015
    ## 895               Florida     Medicare   3535600 2015
    ## 896               Georgia     Medicare   1271300 2015
    ## 897                Hawaii     Medicare    207100 2015
    ## 898                 Idaho     Medicare    232500 2015
    ## 899              Illinois     Medicare   1741700 2015
    ## 900               Indiana     Medicare    906800 2015
    ## 901                  Iowa     Medicare    459600 2015
    ## 902                Kansas     Medicare    370600 2015
    ## 903              Kentucky     Medicare    719500 2015
    ## 904             Louisiana     Medicare    615600 2015
    ## 905                 Maine     Medicare    245600 2015
    ## 906              Maryland     Medicare    705500 2015
    ## 907         Massachusetts     Medicare    792600 2015
    ## 908              Michigan     Medicare   1541800 2015
    ## 909             Minnesota     Medicare    824900 2015
    ## 910           Mississippi     Medicare    428400 2015
    ## 911              Missouri     Medicare    865900 2015
    ## 912               Montana     Medicare    177500 2015
    ## 913              Nebraska     Medicare    241400 2015
    ## 914                Nevada     Medicare    381400 2015
    ## 915         New Hampshire     Medicare    184500 2015
    ## 916            New Jersey     Medicare   1136500 2015
    ## 917            New Mexico     Medicare    313200 2015
    ## 918              New York     Medicare   2584300 2015
    ## 919        North Carolina     Medicare   1288500 2015
    ## 920          North Dakota     Medicare    104600 2015
    ## 921                  Ohio     Medicare   1681500 2015
    ## 922              Oklahoma     Medicare    550200 2015
    ## 923                Oregon     Medicare    578600 2015
    ## 924          Pennsylvania     Medicare   1975500 2015
    ## 925          Rhode Island     Medicare    135100 2015
    ## 926        South Carolina     Medicare    790800 2015
    ## 927          South Dakota     Medicare    115500 2015
    ## 928             Tennessee     Medicare   1052200 2015
    ## 929                 Texas     Medicare   3059800 2015
    ## 930                  Utah     Medicare    307600 2015
    ## 931               Vermont     Medicare     87900 2015
    ## 932              Virginia     Medicare   1168000 2015
    ## 933            Washington     Medicare   1016400 2015
    ## 934         West Virginia     Medicare    340200 2015
    ## 935             Wisconsin     Medicare    813600 2015
    ## 936               Wyoming     Medicare     78100 2015
    ## 937         United States Other.Public   6422300 2015
    ## 938               Alabama Other.Public    174600 2015
    ## 939                Alaska Other.Public     47700 2015
    ## 940               Arizona Other.Public    189300 2015
    ## 941              Arkansas Other.Public     63700 2015
    ## 942            California Other.Public    752700 2015
    ## 943              Colorado Other.Public    148000 2015
    ## 944           Connecticut Other.Public       N/A 2015
    ## 945              Delaware Other.Public     26500 2015
    ## 946  District of Columbia Other.Public       N/A 2015
    ## 947               Florida Other.Public    563200 2015
    ## 948               Georgia Other.Public    275800 2015
    ## 949                Hawaii Other.Public    106500 2015
    ## 950                 Idaho Other.Public     24200 2015
    ## 951              Illinois Other.Public    106900 2015
    ## 952               Indiana Other.Public     74900 2015
    ## 953                  Iowa Other.Public     45000 2015
    ## 954                Kansas Other.Public     62600 2015
    ## 955              Kentucky Other.Public       N/A 2015
    ## 956             Louisiana Other.Public       N/A 2015
    ## 957                 Maine Other.Public     24900 2015
    ## 958              Maryland Other.Public    141200 2015
    ## 959         Massachusetts Other.Public     64300 2015
    ## 960              Michigan Other.Public       N/A 2015
    ## 961             Minnesota Other.Public     52800 2015
    ## 962           Mississippi Other.Public     82100 2015
    ## 963              Missouri Other.Public     67900 2015
    ## 964               Montana Other.Public     37600 2015
    ## 965              Nebraska Other.Public       N/A 2015
    ## 966                Nevada Other.Public    143500 2015
    ## 967         New Hampshire Other.Public       N/A 2015
    ## 968            New Jersey Other.Public       N/A 2015
    ## 969            New Mexico Other.Public     66500 2015
    ## 970              New York Other.Public       N/A 2015
    ## 971        North Carolina Other.Public       N/A 2015
    ## 972          North Dakota Other.Public     19700 2015
    ## 973                  Ohio Other.Public    126400 2015
    ## 974              Oklahoma Other.Public    122500 2015
    ## 975                Oregon Other.Public     64800 2015
    ## 976          Pennsylvania Other.Public     93200 2015
    ## 977          Rhode Island Other.Public       N/A 2015
    ## 978        South Carolina Other.Public    106000 2015
    ## 979          South Dakota Other.Public       N/A 2015
    ## 980             Tennessee Other.Public       N/A 2015
    ## 981                 Texas Other.Public    726000 2015
    ## 982                  Utah Other.Public       N/A 2015
    ## 983               Vermont Other.Public     16500 2015
    ## 984              Virginia Other.Public    375600 2015
    ## 985            Washington Other.Public    116800 2015
    ## 986         West Virginia Other.Public     26500 2015
    ## 987             Wisconsin Other.Public     65000 2015
    ## 988               Wyoming Other.Public     23900 2015
    ## 989         United States    Uninsured  28965900 2015
    ## 990               Alabama    Uninsured    519400 2015
    ## 991                Alaska    Uninsured     90500 2015
    ## 992               Arizona    Uninsured    844800 2015
    ## 993              Arkansas    Uninsured    268400 2015
    ## 994            California    Uninsured   2980600 2015
    ## 995              Colorado    Uninsured    475700 2015
    ## 996           Connecticut    Uninsured    216900 2015
    ## 997              Delaware    Uninsured     65300 2015
    ## 998  District of Columbia    Uninsured     27200 2015
    ## 999               Florida    Uninsured   2544700 2015
    ## 1000              Georgia    Uninsured   1411000 2015
    ## 1001               Hawaii    Uninsured     70300 2015
    ## 1002                Idaho    Uninsured    187500 2015
    ## 1003             Illinois    Uninsured    792300 2015
    ## 1004              Indiana    Uninsured    611000 2015
    ## 1005                 Iowa    Uninsured    166700 2015
    ## 1006               Kansas    Uninsured    285300 2015
    ## 1007             Kentucky    Uninsured    266900 2015
    ## 1008            Louisiana    Uninsured    502900 2015
    ## 1009                Maine    Uninsured     60800 2015
    ## 1010             Maryland    Uninsured    394300 2015
    ## 1011        Massachusetts    Uninsured    288800 2015
    ## 1012             Michigan    Uninsured    571200 2015
    ## 1013            Minnesota    Uninsured    334100 2015
    ## 1014          Mississippi    Uninsured    380000 2015
    ## 1015             Missouri    Uninsured    527000 2015
    ## 1016              Montana    Uninsured    102800 2015
    ## 1017             Nebraska    Uninsured    157900 2015
    ## 1018               Nevada    Uninsured    324800 2015
    ## 1019        New Hampshire    Uninsured     68100 2015
    ## 1020           New Jersey    Uninsured    699400 2015
    ## 1021           New Mexico    Uninsured    252100 2015
    ## 1022             New York    Uninsured   1264200 2015
    ## 1023       North Carolina    Uninsured   1094800 2015
    ## 1024         North Dakota    Uninsured     61800 2015
    ## 1025                 Ohio    Uninsured    681400 2015
    ## 1026             Oklahoma    Uninsured    513900 2015
    ## 1027               Oregon    Uninsured    287300 2015
    ## 1028         Pennsylvania    Uninsured    728000 2015
    ## 1029         Rhode Island    Uninsured     49500 2015
    ## 1030       South Carolina    Uninsured    518900 2015
    ## 1031         South Dakota    Uninsured     78900 2015
    ## 1032            Tennessee    Uninsured    718100 2015
    ## 1033                Texas    Uninsured   4333600 2015
    ## 1034                 Utah    Uninsured    301800 2015
    ## 1035              Vermont    Uninsured     31500 2015
    ## 1036             Virginia    Uninsured    763400 2015
    ## 1037           Washington    Uninsured    493200 2015
    ## 1038        West Virginia    Uninsured    116200 2015
    ## 1039            Wisconsin    Uninsured    387400 2015
    ## 1040              Wyoming    Uninsured     53400 2015
    ## 1041        United States        Total 318868500 2015
    ## 1042              Alabama        Total   4833900 2015
    ## 1043               Alaska        Total    705300 2015
    ## 1044              Arizona        Total   6739500 2015
    ## 1045             Arkansas        Total   2953000 2015
    ## 1046           California        Total  39113900 2015
    ## 1047             Colorado        Total   5421300 2015
    ## 1048          Connecticut        Total   3571700 2015
    ## 1049             Delaware        Total    959100 2015
    ## 1050 District of Columbia        Total    676800 2015
    ## 1051              Florida        Total  20085300 2015
    ## 1052              Georgia        Total  10104900 2015
    ## 1053               Hawaii        Total   1386000 2015
    ## 1054                Idaho        Total   1659500 2015
    ## 1055             Illinois        Total  12701800 2015
    ## 1056              Indiana        Total   6512100 2015
    ## 1057                 Iowa        Total   3100600 2015
    ## 1058               Kansas        Total   2852400 2015
    ## 1059             Kentucky        Total   4383400 2015
    ## 1060            Louisiana        Total   4604200 2015
    ## 1061                Maine        Total   1341900 2015
    ## 1062             Maryland        Total   5900500 2015
    ## 1063        Massachusetts        Total   6785700 2015
    ## 1064             Michigan        Total   9862100 2015
    ## 1065            Minnesota        Total   5463000 2015
    ## 1066          Mississippi        Total   2948600 2015
    ## 1067             Missouri        Total   5962700 2015
    ## 1068              Montana        Total   1018100 2015
    ## 1069             Nebraska        Total   1859800 2015
    ## 1070               Nevada        Total   2867400 2015
    ## 1071        New Hampshire        Total   1292800 2015
    ## 1072           New Jersey        Total   8941600 2015
    ## 1073           New Mexico        Total   2041000 2015
    ## 1074             New York        Total  19695000 2015
    ## 1075       North Carolina        Total   9902000 2015
    ## 1076         North Dakota        Total    763400 2015
    ## 1077                 Ohio        Total  11450900 2015
    ## 1078             Oklahoma        Total   3902900 2015
    ## 1079               Oregon        Total   4032800 2015
    ## 1080         Pennsylvania        Total  12595900 2015
    ## 1081         Rhode Island        Total   1044800 2015
    ## 1082       South Carolina        Total   4794700 2015
    ## 1083         South Dakota        Total    848400 2015
    ## 1084            Tennessee        Total   6616500 2015
    ## 1085                Texas        Total  27434400 2015
    ## 1086                 Utah        Total   3004500 2015
    ## 1087              Vermont        Total    609700 2015
    ## 1088             Virginia        Total   8217200 2015
    ## 1089           Washington        Total   7194700 2015
    ## 1090        West Virginia        Total   1797500 2015
    ## 1091            Wisconsin        Total   5738100 2015
    ## 1092              Wyoming        Total    574800 2015
    ## 1093        United States     Employer 157381500 2016
    ## 1094              Alabama     Employer   2263800 2016
    ## 1095               Alaska     Employer    324400 2016
    ## 1096              Arizona     Employer   3010700 2016
    ## 1097             Arkansas     Employer   1290900 2016
    ## 1098           California     Employer  18116200 2016
    ## 1099             Colorado     Employer   2872600 2016
    ## 1100          Connecticut     Employer   1926900 2016
    ## 1101             Delaware     Employer    445500 2016
    ## 1102 District of Columbia     Employer    351800 2016
    ## 1103              Florida     Employer   8602700 2016
    ## 1104              Georgia     Employer   5038000 2016
    ## 1105               Hawaii     Employer    771400 2016
    ## 1106                Idaho     Employer    811900 2016
    ## 1107             Illinois     Employer   6491400 2016
    ## 1108              Indiana     Employer   3379100 2016
    ## 1109                 Iowa     Employer   1690700 2016
    ## 1110               Kansas     Employer   1506300 2016
    ## 1111             Kentucky     Employer   1948800 2016
    ## 1112            Louisiana     Employer   1910100 2016
    ## 1113                Maine     Employer    645400 2016
    ## 1114             Maryland     Employer   3210600 2016
    ## 1115        Massachusetts     Employer   3666500 2016
    ## 1116             Michigan     Employer   5046700 2016
    ## 1117            Minnesota     Employer   3010000 2016
    ## 1118          Mississippi     Employer   1233800 2016
    ## 1119             Missouri     Employer   3021200 2016
    ## 1120              Montana     Employer    448700 2016
    ## 1121             Nebraska     Employer   1039100 2016
    ## 1122               Nevada     Employer   1458200 2016
    ## 1123        New Hampshire     Employer    761800 2016
    ## 1124           New Jersey     Employer   4838400 2016
    ## 1125           New Mexico     Employer    740300 2016
    ## 1126             New York     Employer   9767500 2016
    ## 1127       North Carolina     Employer   4570100 2016
    ## 1128         North Dakota     Employer    415000 2016
    ## 1129                 Ohio     Employer   5847600 2016
    ## 1130             Oklahoma     Employer   1885900 2016
    ## 1131               Oregon     Employer   1910600 2016
    ## 1132         Pennsylvania     Employer   6663200 2016
    ## 1133         Rhode Island     Employer    535000 2016
    ## 1134       South Carolina     Employer   2257300 2016
    ## 1135         South Dakota     Employer    416500 2016
    ## 1136            Tennessee     Employer   3072000 2016
    ## 1137                Texas     Employer  13607200 2016
    ## 1138                 Utah     Employer   1854700 2016
    ## 1139              Vermont     Employer    292700 2016
    ## 1140             Virginia     Employer   4472900 2016
    ## 1141           Washington     Employer   3593400 2016
    ## 1142        West Virginia     Employer    788500 2016
    ## 1143            Wisconsin     Employer   3245000 2016
    ## 1144              Wyoming     Employer    312200 2016
    ## 1145        United States    Non.Group  21884400 2016
    ## 1146              Alabama    Non.Group    262400 2016
    ## 1147               Alaska    Non.Group     20300 2016
    ## 1148              Arizona    Non.Group    377000 2016
    ## 1149             Arkansas    Non.Group    252900 2016
    ## 1150           California    Non.Group   3195400 2016
    ## 1151             Colorado    Non.Group    370000 2016
    ## 1152          Connecticut    Non.Group    200700 2016
    ## 1153             Delaware    Non.Group     36200 2016
    ## 1154 District of Columbia    Non.Group     48100 2016
    ## 1155              Florida    Non.Group   2061400 2016
    ## 1156              Georgia    Non.Group    695300 2016
    ## 1157               Hawaii    Non.Group     47500 2016
    ## 1158                Idaho    Non.Group    164600 2016
    ## 1159             Illinois    Non.Group    965000 2016
    ## 1160              Indiana    Non.Group     4e+05 2016
    ## 1161                 Iowa    Non.Group    191800 2016
    ## 1162               Kansas    Non.Group    246300 2016
    ## 1163             Kentucky    Non.Group    428000 2016
    ## 1164            Louisiana    Non.Group    281500 2016
    ## 1165                Maine    Non.Group     65600 2016
    ## 1166             Maryland    Non.Group    443000 2016
    ## 1167        Massachusetts    Non.Group    392300 2016
    ## 1168             Michigan    Non.Group    456400 2016
    ## 1169            Minnesota    Non.Group    444500 2016
    ## 1170          Mississippi    Non.Group    159300 2016
    ## 1171             Missouri    Non.Group    467500 2016
    ## 1172              Montana    Non.Group     72100 2016
    ## 1173             Nebraska    Non.Group    140200 2016
    ## 1174               Nevada    Non.Group    152000 2016
    ## 1175        New Hampshire    Non.Group     73700 2016
    ## 1176           New Jersey    Non.Group    545300 2016
    ## 1177           New Mexico    Non.Group    102800 2016
    ## 1178             New York    Non.Group   1294200 2016
    ## 1179       North Carolina    Non.Group    863500 2016
    ## 1180         North Dakota    Non.Group     58500 2016
    ## 1181                 Ohio    Non.Group    627500 2016
    ## 1182             Oklahoma    Non.Group    192400 2016
    ## 1183               Oregon    Non.Group    286000 2016
    ## 1184         Pennsylvania    Non.Group    721300 2016
    ## 1185         Rhode Island    Non.Group     83700 2016
    ## 1186       South Carolina    Non.Group    324900 2016
    ## 1187         South Dakota    Non.Group     82100 2016
    ## 1188            Tennessee    Non.Group    370700 2016
    ## 1189                Texas    Non.Group   1662100 2016
    ## 1190                 Utah    Non.Group    203700 2016
    ## 1191              Vermont    Non.Group     55700 2016
    ## 1192             Virginia    Non.Group    418700 2016
    ## 1193           Washington    Non.Group    498500 2016
    ## 1194        West Virginia    Non.Group     64000 2016
    ## 1195            Wisconsin    Non.Group    280600 2016
    ## 1196              Wyoming    Non.Group     37400 2016
    ## 1197        United States     Medicaid  62303400 2016
    ## 1198              Alabama     Medicaid    997000 2016
    ## 1199               Alaska     Medicaid    145400 2016
    ## 1200              Arizona     Medicaid   1468400 2016
    ## 1201             Arkansas     Medicaid    618600 2016
    ## 1202           California     Medicaid   9853800 2016
    ## 1203             Colorado     Medicaid    855800 2016
    ## 1204          Connecticut     Medicaid    711200 2016
    ## 1205             Delaware     Medicaid    211900 2016
    ## 1206 District of Columbia     Medicaid    166000 2016
    ## 1207              Florida     Medicaid   3294400 2016
    ## 1208              Georgia     Medicaid   1708000 2016
    ## 1209               Hawaii     Medicaid    207900 2016
    ## 1210                Idaho     Medicaid    293500 2016
    ## 1211             Illinois     Medicaid   2321100 2016
    ## 1212              Indiana     Medicaid   1279300 2016
    ## 1213                 Iowa     Medicaid    571300 2016
    ## 1214               Kansas     Medicaid    412200 2016
    ## 1215             Kentucky     Medicaid    928000 2016
    ## 1216            Louisiana     Medicaid   1136600 2016
    ## 1217                Maine     Medicaid    280700 2016
    ## 1218             Maryland     Medicaid    926300 2016
    ## 1219        Massachusetts     Medicaid   1473500 2016
    ## 1220             Michigan     Medicaid   2168900 2016
    ## 1221            Minnesota     Medicaid    787500 2016
    ## 1222          Mississippi     Medicaid    696400 2016
    ## 1223             Missouri     Medicaid    871500 2016
    ## 1224              Montana     Medicaid    222700 2016
    ## 1225             Nebraska     Medicaid    244500 2016
    ## 1226               Nevada     Medicaid    515500 2016
    ## 1227        New Hampshire     Medicaid    165300 2016
    ## 1228           New Jersey     Medicaid   1492700 2016
    ## 1229           New Mexico     Medicaid    641300 2016
    ## 1230             New York     Medicaid   4621700 2016
    ## 1231       North Carolina     Medicaid   1787900 2016
    ## 1232         North Dakota     Medicaid     91300 2016
    ## 1233                 Ohio     Medicaid   2506900 2016
    ## 1234             Oklahoma     Medicaid    688200 2016
    ## 1235               Oregon     Medicaid   1009900 2016
    ## 1236         Pennsylvania     Medicaid   2379500 2016
    ## 1237         Rhode Island     Medicaid    214100 2016
    ## 1238       South Carolina     Medicaid    916600 2016
    ## 1239         South Dakota     Medicaid    126900 2016
    ## 1240            Tennessee     Medicaid   1337600 2016
    ## 1241                Texas     Medicaid   4513800 2016
    ## 1242                 Utah     Medicaid    308100 2016
    ## 1243              Vermont     Medicaid    124800 2016
    ## 1244             Virginia     Medicaid    948100 2016
    ## 1245           Washington     Medicaid   1608200 2016
    ## 1246        West Virginia     Medicaid    470900 2016
    ## 1247            Wisconsin     Medicaid    910100 2016
    ## 1248              Wyoming     Medicaid     72100 2016
    ## 1249        United States     Medicare  44550200 2016
    ## 1250              Alabama     Medicare    761200 2016
    ## 1251               Alaska     Medicare     68200 2016
    ## 1252              Arizona     Medicare   1028000 2016
    ## 1253             Arkansas     Medicare    490000 2016
    ## 1254           California     Medicare   4436000 2016
    ## 1255             Colorado     Medicare    692400 2016
    ## 1256          Connecticut     Medicare    483300 2016
    ## 1257             Delaware     Medicare    152000 2016
    ## 1258 District of Columbia     Medicare     76000 2016
    ## 1259              Florida     Medicare   3602900 2016
    ## 1260              Georgia     Medicare   1213800 2016
    ## 1261               Hawaii     Medicare    210200 2016
    ## 1262                Idaho     Medicare    229500 2016
    ## 1263             Illinois     Medicare   1761300 2016
    ## 1264              Indiana     Medicare    922500 2016
    ## 1265                 Iowa     Medicare    459800 2016
    ## 1266               Kansas     Medicare    380200 2016
    ## 1267             Kentucky     Medicare    706300 2016
    ## 1268            Louisiana     Medicare    621100 2016
    ## 1269                Maine     Medicare    217100 2016
    ## 1270             Maryland     Medicare    827000 2016
    ## 1271        Massachusetts     Medicare    797400 2016
    ## 1272             Michigan     Medicare   1522400 2016
    ## 1273            Minnesota     Medicare    821100 2016
    ## 1274          Mississippi     Medicare    418600 2016
    ## 1275             Missouri     Medicare    989100 2016
    ## 1276              Montana     Medicare    183700 2016
    ## 1277             Nebraska     Medicare    280900 2016
    ## 1278               Nevada     Medicare    454700 2016
    ## 1279        New Hampshire     Medicare    209300 2016
    ## 1280           New Jersey     Medicare   1240700 2016
    ## 1281           New Mexico     Medicare    281200 2016
    ## 1282             New York     Medicare   2517200 2016
    ## 1283       North Carolina     Medicare   1478000 2016
    ## 1284         North Dakota     Medicare    100200 2016
    ## 1285                 Ohio     Medicare   1753800 2016
    ## 1286             Oklahoma     Medicare    622800 2016
    ## 1287               Oregon     Medicare    650500 2016
    ## 1288         Pennsylvania     Medicare   2040900 2016
    ## 1289         Rhode Island     Medicare    156800 2016
    ## 1290       South Carolina     Medicare    766800 2016
    ## 1291         South Dakota     Medicare    143200 2016
    ## 1292            Tennessee     Medicare    985500 2016
    ## 1293                Texas     Medicare   3083900 2016
    ## 1294                 Utah     Medicare    306300 2016
    ## 1295              Vermont     Medicare    100600 2016
    ## 1296             Virginia     Medicare   1127600 2016
    ## 1297           Washington     Medicare    961800 2016
    ## 1298        West Virginia     Medicare    332600 2016
    ## 1299            Wisconsin     Medicare    805700 2016
    ## 1300              Wyoming     Medicare     78000 2016
    ## 1301        United States Other.Public   6192200 2016
    ## 1302              Alabama Other.Public    128800 2016
    ## 1303               Alaska Other.Public     55600 2016
    ## 1304              Arizona Other.Public    172500 2016
    ## 1305             Arkansas Other.Public     67500 2016
    ## 1306           California Other.Public    556100 2016
    ## 1307             Colorado Other.Public    190100 2016
    ## 1308          Connecticut Other.Public       N/A 2016
    ## 1309             Delaware Other.Public     18600 2016
    ## 1310 District of Columbia Other.Public       N/A 2016
    ## 1311              Florida Other.Public    518200 2016
    ## 1312              Georgia Other.Public    360000 2016
    ## 1313               Hawaii Other.Public     80400 2016
    ## 1314                Idaho Other.Public     32500 2016
    ## 1315             Illinois Other.Public    137600 2016
    ## 1316              Indiana Other.Public     94300 2016
    ## 1317                 Iowa Other.Public     30200 2016
    ## 1318               Kansas Other.Public       N/A 2016
    ## 1319             Kentucky Other.Public       N/A 2016
    ## 1320            Louisiana Other.Public       N/A 2016
    ## 1321                Maine Other.Public     19000 2016
    ## 1322             Maryland Other.Public    153800 2016
    ## 1323        Massachusetts Other.Public     70800 2016
    ## 1324             Michigan Other.Public     72500 2016
    ## 1325            Minnesota Other.Public       N/A 2016
    ## 1326          Mississippi Other.Public     88100 2016
    ## 1327             Missouri Other.Public     53000 2016
    ## 1328              Montana Other.Public     35200 2016
    ## 1329             Nebraska Other.Public     41300 2016
    ## 1330               Nevada Other.Public    107200 2016
    ## 1331        New Hampshire Other.Public     21700 2016
    ## 1332           New Jersey Other.Public       N/A 2016
    ## 1333           New Mexico Other.Public     49500 2016
    ## 1334             New York Other.Public    183400 2016
    ## 1335       North Carolina Other.Public    281800 2016
    ## 1336         North Dakota Other.Public     26700 2016
    ## 1337                 Ohio Other.Public     95000 2016
    ## 1338             Oklahoma Other.Public    120000 2016
    ## 1339               Oregon Other.Public     54800 2016
    ## 1340         Pennsylvania Other.Public    151500 2016
    ## 1341         Rhode Island Other.Public       N/A 2016
    ## 1342       South Carolina Other.Public    194600 2016
    ## 1343         South Dakota Other.Public     19700 2016
    ## 1344            Tennessee Other.Public       N/A 2016
    ## 1345                Texas Other.Public    565100 2016
    ## 1346                 Utah Other.Public       N/A 2016
    ## 1347              Vermont Other.Public     14600 2016
    ## 1348             Virginia Other.Public    393900 2016
    ## 1349           Washington Other.Public    121700 2016
    ## 1350        West Virginia Other.Public     24900 2016
    ## 1351            Wisconsin Other.Public    106400 2016
    ## 1352              Wyoming Other.Public     16400 2016
    ## 1353        United States    Uninsured  28051900 2016
    ## 1354              Alabama    Uninsured    420800 2016
    ## 1355               Alaska    Uninsured     96900 2016
    ## 1356              Arizona    Uninsured    833700 2016
    ## 1357             Arkansas    Uninsured    225500 2016
    ## 1358           California    Uninsured   3030800 2016
    ## 1359             Colorado    Uninsured    528400 2016
    ## 1360          Connecticut    Uninsured    218600 2016
    ## 1361             Delaware    Uninsured     84100 2016
    ## 1362 District of Columbia    Uninsured     35400 2016
    ## 1363              Florida    Uninsured   2465800 2016
    ## 1364              Georgia    Uninsured   1265600 2016
    ## 1365               Hawaii    Uninsured     75000 2016
    ## 1366                Idaho    Uninsured    147100 2016
    ## 1367             Illinois    Uninsured    931100 2016
    ## 1368              Indiana    Uninsured    428100 2016
    ## 1369                 Iowa    Uninsured    162300 2016
    ## 1370               Kansas    Uninsured    241900 2016
    ## 1371             Kentucky    Uninsured    271400 2016
    ## 1372            Louisiana    Uninsured    484500 2016
    ## 1373                Maine    Uninsured     93900 2016
    ## 1374             Maryland    Uninsured    372100 2016
    ## 1375        Massachusetts    Uninsured    379100 2016
    ## 1376             Michigan    Uninsured    626300 2016
    ## 1377            Minnesota    Uninsured    321200 2016
    ## 1378          Mississippi    Uninsured    351900 2016
    ## 1379             Missouri    Uninsured    486300 2016
    ## 1380              Montana    Uninsured     72700 2016
    ## 1381             Nebraska    Uninsured    130100 2016
    ## 1382               Nevada    Uninsured    255600 2016
    ## 1383        New Hampshire    Uninsured     85800 2016
    ## 1384           New Jersey    Uninsured    696200 2016
    ## 1385           New Mexico    Uninsured    230000 2016
    ## 1386             New York    Uninsured   1094400 2016
    ## 1387       North Carolina    Uninsured   1087000 2016
    ## 1388         North Dakota    Uninsured     57800 2016
    ## 1389                 Ohio    Uninsured    638000 2016
    ## 1390             Oklahoma    Uninsured    411800 2016
    ## 1391               Oregon    Uninsured    218400 2016
    ## 1392         Pennsylvania    Uninsured    601500 2016
    ## 1393         Rhode Island    Uninsured     50500 2016
    ## 1394       South Carolina    Uninsured    448200 2016
    ## 1395         South Dakota    Uninsured     67000 2016
    ## 1396            Tennessee    Uninsured    744800 2016
    ## 1397                Texas    Uninsured   4245000 2016
    ## 1398                 Utah    Uninsured    373900 2016
    ## 1399              Vermont    Uninsured     33900 2016
    ## 1400             Virginia    Uninsured    813900 2016
    ## 1401           Washington    Uninsured    513800 2016
    ## 1402        West Virginia    Uninsured    133100 2016
    ## 1403            Wisconsin    Uninsured    415000 2016
    ## 1404              Wyoming    Uninsured     55500 2016
    ## 1405        United States        Total 320372000 2016
    ## 1406              Alabama        Total   4834100 2016
    ## 1407               Alaska        Total    710800 2016
    ## 1408              Arizona        Total   6890200 2016
    ## 1409             Arkansas        Total   2945300 2016
    ## 1410           California        Total  39188300 2016
    ## 1411             Colorado        Total   5509200 2016
    ## 1412          Connecticut        Total   3570300 2016
    ## 1413             Delaware        Total    948200 2016
    ## 1414 District of Columbia        Total    685800 2016
    ## 1415              Florida        Total  20545300 2016
    ## 1416              Georgia        Total  10280800 2016
    ## 1417               Hawaii        Total   1392400 2016
    ## 1418                Idaho        Total   1679000 2016
    ## 1419             Illinois        Total  12607400 2016
    ## 1420              Indiana        Total   6503200 2016
    ## 1421                 Iowa        Total   3106200 2016
    ## 1422               Kansas        Total   2865000 2016
    ## 1423             Kentucky        Total   4388200 2016
    ## 1424            Louisiana        Total   4578500 2016
    ## 1425                Maine        Total   1321600 2016
    ## 1426             Maryland        Total   5932800 2016
    ## 1427        Massachusetts        Total   6779600 2016
    ## 1428             Michigan        Total   9893200 2016
    ## 1429            Minnesota        Total   5436600 2016
    ## 1430          Mississippi        Total   2948100 2016
    ## 1431             Missouri        Total   5888700 2016
    ## 1432              Montana        Total   1035500 2016
    ## 1433             Nebraska        Total   1876100 2016
    ## 1434               Nevada        Total   2943200 2016
    ## 1435        New Hampshire        Total   1318100 2016
    ## 1436           New Jersey        Total   8851500 2016
    ## 1437           New Mexico        Total   2045000 2016
    ## 1438             New York        Total  19482300 2016
    ## 1439       North Carolina        Total  10068300 2016
    ## 1440         North Dakota        Total    749500 2016
    ## 1441                 Ohio        Total  11468700 2016
    ## 1442             Oklahoma        Total   3921100 2016
    ## 1443               Oregon        Total   4130200 2016
    ## 1444         Pennsylvania        Total  12557900 2016
    ## 1445         Rhode Island        Total   1054300 2016
    ## 1446       South Carolina        Total   4908400 2016
    ## 1447         South Dakota        Total    855400 2016
    ## 1448            Tennessee        Total   6674100 2016
    ## 1449                Texas        Total  27677200 2016
    ## 1450                 Utah        Total   3079700 2016
    ## 1451              Vermont        Total    622500 2016
    ## 1452             Virginia        Total   8175000 2016
    ## 1453           Washington        Total   7297300 2016
    ## 1454        West Virginia        Total   1814100 2016
    ## 1455            Wisconsin        Total   5766100 2016
    ## 1456              Wyoming        Total    571700 2016

``` r
tidy.expenditures
```

    ##                  Location              Category Total.Amount Year
    ## 1           United States Total.Health.Spending       675896 1991
    ## 2                 Alabama Total.Health.Spending        10393 1991
    ## 3                  Alaska Total.Health.Spending         1458 1991
    ## 4                 Arizona Total.Health.Spending         9269 1991
    ## 5                Arkansas Total.Health.Spending         5632 1991
    ## 6              California Total.Health.Spending        81438 1991
    ## 7                Colorado Total.Health.Spending         8460 1991
    ## 8             Connecticut Total.Health.Spending        10950 1991
    ## 9                Delaware Total.Health.Spending         1938 1991
    ## 10   District of Columbia Total.Health.Spending         2800 1991
    ## 11                Florida Total.Health.Spending        39430 1991
    ## 12                Georgia Total.Health.Spending        16612 1991
    ## 13                 Hawaii Total.Health.Spending         2959 1991
    ## 14                  Idaho Total.Health.Spending         2213 1991
    ## 15               Illinois Total.Health.Spending        31577 1991
    ## 16                Indiana Total.Health.Spending        14272 1991
    ## 17                   Iowa Total.Health.Spending         7248 1991
    ## 18                 Kansas Total.Health.Spending         6529 1991
    ## 19               Kentucky Total.Health.Spending         8877 1991
    ## 20              Louisiana Total.Health.Spending        11259 1991
    ## 21                  Maine Total.Health.Spending         3152 1991
    ## 22               Maryland Total.Health.Spending        13560 1991
    ## 23          Massachusetts Total.Health.Spending        20015 1991
    ## 24               Michigan Total.Health.Spending        25038 1991
    ## 25              Minnesota Total.Health.Spending        11981 1991
    ## 26            Mississippi Total.Health.Spending         5650 1991
    ## 27               Missouri Total.Health.Spending        13012 1991
    ## 28                Montana Total.Health.Spending         1907 1991
    ## 29               Nebraska Total.Health.Spending         3790 1991
    ## 30                 Nevada Total.Health.Spending         3086 1991
    ## 31          New Hampshire Total.Health.Spending         2727 1991
    ## 32             New Jersey Total.Health.Spending        23100 1991
    ## 33             New Mexico Total.Health.Spending         3483 1991
    ## 34               New York Total.Health.Spending        58080 1991
    ## 35         North Carolina Total.Health.Spending        15352 1991
    ## 36           North Dakota Total.Health.Spending         1709 1991
    ## 37                   Ohio Total.Health.Spending        29348 1991
    ## 38               Oklahoma Total.Health.Spending         7512 1991
    ## 39                 Oregon Total.Health.Spending         6760 1991
    ## 40           Pennsylvania Total.Health.Spending        36171 1991
    ## 41           Rhode Island Total.Health.Spending         2967 1991
    ## 42         South Carolina Total.Health.Spending         8075 1991
    ## 43           South Dakota Total.Health.Spending         1707 1991
    ## 44              Tennessee Total.Health.Spending        12692 1991
    ## 45                  Texas Total.Health.Spending        40967 1991
    ## 46                   Utah Total.Health.Spending         3522 1991
    ## 47                Vermont Total.Health.Spending         1330 1991
    ## 48               Virginia Total.Health.Spending        14829 1991
    ## 49             Washington Total.Health.Spending        12674 1991
    ## 50          West Virginia Total.Health.Spending         4672 1991
    ## 51              Wisconsin Total.Health.Spending        12694 1991
    ## 52                Wyoming Total.Health.Spending         1023 1991
    ## 53          United States Total.Health.Spending       731455 1992
    ## 54                Alabama Total.Health.Spending        11284 1992
    ## 55                 Alaska Total.Health.Spending         1558 1992
    ## 56                Arizona Total.Health.Spending         9815 1992
    ## 57               Arkansas Total.Health.Spending         6022 1992
    ## 58             California Total.Health.Spending        87949 1992
    ## 59               Colorado Total.Health.Spending         9215 1992
    ## 60            Connecticut Total.Health.Spending        11635 1992
    ## 61               Delaware Total.Health.Spending         2111 1992
    ## 62   District of Columbia Total.Health.Spending         3098 1992
    ## 63                Florida Total.Health.Spending        43041 1992
    ## 64                Georgia Total.Health.Spending        18207 1992
    ## 65                 Hawaii Total.Health.Spending         3163 1992
    ## 66                  Idaho Total.Health.Spending         2415 1992
    ## 67               Illinois Total.Health.Spending        34119 1992
    ## 68                Indiana Total.Health.Spending        15552 1992
    ## 69                   Iowa Total.Health.Spending         7704 1992
    ## 70                 Kansas Total.Health.Spending         7001 1992
    ## 71               Kentucky Total.Health.Spending         9697 1992
    ## 72              Louisiana Total.Health.Spending        12286 1992
    ## 73                  Maine Total.Health.Spending         3326 1992
    ## 74               Maryland Total.Health.Spending        14531 1992
    ## 75          Massachusetts Total.Health.Spending        21268 1992
    ## 76               Michigan Total.Health.Spending        26437 1992
    ## 77              Minnesota Total.Health.Spending        12598 1992
    ## 78            Mississippi Total.Health.Spending         6060 1992
    ## 79               Missouri Total.Health.Spending        14242 1992
    ## 80                Montana Total.Health.Spending         2071 1992
    ## 81               Nebraska Total.Health.Spending         4113 1992
    ## 82                 Nevada Total.Health.Spending         3464 1992
    ## 83          New Hampshire Total.Health.Spending         2997 1992
    ## 84             New Jersey Total.Health.Spending        25192 1992
    ## 85             New Mexico Total.Health.Spending         3823 1992
    ## 86               New York Total.Health.Spending        62675 1992
    ## 87         North Carolina Total.Health.Spending        16855 1992
    ## 88           North Dakota Total.Health.Spending         1845 1992
    ## 89                   Ohio Total.Health.Spending        31928 1992
    ## 90               Oklahoma Total.Health.Spending         8178 1992
    ## 91                 Oregon Total.Health.Spending         7458 1992
    ## 92           Pennsylvania Total.Health.Spending        39165 1992
    ## 93           Rhode Island Total.Health.Spending         3205 1992
    ## 94         South Carolina Total.Health.Spending         8830 1992
    ## 95           South Dakota Total.Health.Spending         1825 1992
    ## 96              Tennessee Total.Health.Spending        13979 1992
    ## 97                  Texas Total.Health.Spending        44949 1992
    ## 98                   Utah Total.Health.Spending         3795 1992
    ## 99                Vermont Total.Health.Spending         1421 1992
    ## 100              Virginia Total.Health.Spending        15599 1992
    ## 101            Washington Total.Health.Spending        13859 1992
    ## 102         West Virginia Total.Health.Spending         5159 1992
    ## 103             Wisconsin Total.Health.Spending        13669 1992
    ## 104               Wyoming Total.Health.Spending         1067 1992
    ## 105         United States Total.Health.Spending       778684 1993
    ## 106               Alabama Total.Health.Spending        12028 1993
    ## 107                Alaska Total.Health.Spending         1661 1993
    ## 108               Arizona Total.Health.Spending        10655 1993
    ## 109              Arkansas Total.Health.Spending         6397 1993
    ## 110            California Total.Health.Spending        91963 1993
    ## 111              Colorado Total.Health.Spending         9803 1993
    ## 112           Connecticut Total.Health.Spending        12081 1993
    ## 113              Delaware Total.Health.Spending         2285 1993
    ## 114  District of Columbia Total.Health.Spending         3240 1993
    ## 115               Florida Total.Health.Spending        46123 1993
    ## 116               Georgia Total.Health.Spending        19722 1993
    ## 117                Hawaii Total.Health.Spending         3439 1993
    ## 118                 Idaho Total.Health.Spending         2608 1993
    ## 119              Illinois Total.Health.Spending        36158 1993
    ## 120               Indiana Total.Health.Spending        16754 1993
    ## 121                  Iowa Total.Health.Spending         8132 1993
    ## 122                Kansas Total.Health.Spending         7344 1993
    ## 123              Kentucky Total.Health.Spending        10280 1993
    ## 124             Louisiana Total.Health.Spending        13170 1993
    ## 125                 Maine Total.Health.Spending         3567 1993
    ## 126              Maryland Total.Health.Spending        15528 1993
    ## 127         Massachusetts Total.Health.Spending        22863 1993
    ## 128              Michigan Total.Health.Spending        28022 1993
    ## 129             Minnesota Total.Health.Spending        13458 1993
    ## 130           Mississippi Total.Health.Spending         6570 1993
    ## 131              Missouri Total.Health.Spending        15186 1993
    ## 132               Montana Total.Health.Spending         2187 1993
    ## 133              Nebraska Total.Health.Spending         4352 1993
    ## 134                Nevada Total.Health.Spending         3696 1993
    ## 135         New Hampshire Total.Health.Spending         3131 1993
    ## 136            New Jersey Total.Health.Spending        26597 1993
    ## 137            New Mexico Total.Health.Spending         4070 1993
    ## 138              New York Total.Health.Spending        67025 1993
    ## 139        North Carolina Total.Health.Spending        18457 1993
    ## 140          North Dakota Total.Health.Spending         1941 1993
    ## 141                  Ohio Total.Health.Spending        33903 1993
    ## 142              Oklahoma Total.Health.Spending         8559 1993
    ## 143                Oregon Total.Health.Spending         7960 1993
    ## 144          Pennsylvania Total.Health.Spending        41395 1993
    ## 145          Rhode Island Total.Health.Spending         3396 1993
    ## 146        South Carolina Total.Health.Spending         9536 1993
    ## 147          South Dakota Total.Health.Spending         1924 1993
    ## 148             Tennessee Total.Health.Spending        15165 1993
    ## 149                 Texas Total.Health.Spending        48177 1993
    ## 150                  Utah Total.Health.Spending         4143 1993
    ## 151               Vermont Total.Health.Spending         1522 1993
    ## 152              Virginia Total.Health.Spending        16634 1993
    ## 153            Washington Total.Health.Spending        14523 1993
    ## 154         West Virginia Total.Health.Spending         5550 1993
    ## 155             Wisconsin Total.Health.Spending        14636 1993
    ## 156               Wyoming Total.Health.Spending         1171 1993
    ## 157         United States Total.Health.Spending       820172 1994
    ## 158               Alabama Total.Health.Spending        12742 1994
    ## 159                Alaska Total.Health.Spending         1728 1994
    ## 160               Arizona Total.Health.Spending        11364 1994
    ## 161              Arkansas Total.Health.Spending         6810 1994
    ## 162            California Total.Health.Spending        94245 1994
    ## 163              Colorado Total.Health.Spending        10382 1994
    ## 164           Connecticut Total.Health.Spending        12772 1994
    ## 165              Delaware Total.Health.Spending         2489 1994
    ## 166  District of Columbia Total.Health.Spending         3255 1994
    ## 167               Florida Total.Health.Spending        48886 1994
    ## 168               Georgia Total.Health.Spending        20784 1994
    ## 169                Hawaii Total.Health.Spending         3710 1994
    ## 170                 Idaho Total.Health.Spending         2762 1994
    ## 171              Illinois Total.Health.Spending        38139 1994
    ## 172               Indiana Total.Health.Spending        17425 1994
    ## 173                  Iowa Total.Health.Spending         8619 1994
    ## 174                Kansas Total.Health.Spending         7829 1994
    ## 175              Kentucky Total.Health.Spending        10768 1994
    ## 176             Louisiana Total.Health.Spending        13926 1994
    ## 177                 Maine Total.Health.Spending         3719 1994
    ## 178              Maryland Total.Health.Spending        16540 1994
    ## 179         Massachusetts Total.Health.Spending        23792 1994
    ## 180              Michigan Total.Health.Spending        29729 1994
    ## 181             Minnesota Total.Health.Spending        14555 1994
    ## 182           Mississippi Total.Health.Spending         7077 1994
    ## 183              Missouri Total.Health.Spending        16307 1994
    ## 184               Montana Total.Health.Spending         2286 1994
    ## 185              Nebraska Total.Health.Spending         4630 1994
    ## 186                Nevada Total.Health.Spending         4102 1994
    ## 187         New Hampshire Total.Health.Spending         3299 1994
    ## 188            New Jersey Total.Health.Spending        28432 1994
    ## 189            New Mexico Total.Health.Spending         4322 1994
    ## 190              New York Total.Health.Spending        70819 1994
    ## 191        North Carolina Total.Health.Spending        20015 1994
    ## 192          North Dakota Total.Health.Spending         2076 1994
    ## 193                  Ohio Total.Health.Spending        35399 1994
    ## 194              Oklahoma Total.Health.Spending         9187 1994
    ## 195                Oregon Total.Health.Spending         8381 1994
    ## 196          Pennsylvania Total.Health.Spending        43035 1994
    ## 197          Rhode Island Total.Health.Spending         3444 1994
    ## 198        South Carolina Total.Health.Spending        10213 1994
    ## 199          South Dakota Total.Health.Spending         2089 1994
    ## 200             Tennessee Total.Health.Spending        16005 1994
    ## 201                 Texas Total.Health.Spending        50473 1994
    ## 202                  Utah Total.Health.Spending         4359 1994
    ## 203               Vermont Total.Health.Spending         1625 1994
    ## 204              Virginia Total.Health.Spending        17637 1994
    ## 205            Washington Total.Health.Spending        15303 1994
    ## 206         West Virginia Total.Health.Spending         5891 1994
    ## 207             Wisconsin Total.Health.Spending        15532 1994
    ## 208               Wyoming Total.Health.Spending         1265 1994
    ## 209         United States Total.Health.Spending       869578 1995
    ## 210               Alabama Total.Health.Spending        13590 1995
    ## 211                Alaska Total.Health.Spending         1879 1995
    ## 212               Arizona Total.Health.Spending        12042 1995
    ## 213              Arkansas Total.Health.Spending         7343 1995
    ## 214            California Total.Health.Spending        96870 1995
    ## 215              Colorado Total.Health.Spending        11153 1995
    ## 216           Connecticut Total.Health.Spending        13649 1995
    ## 217              Delaware Total.Health.Spending         2655 1995
    ## 218  District of Columbia Total.Health.Spending         3285 1995
    ## 219               Florida Total.Health.Spending        51568 1995
    ## 220               Georgia Total.Health.Spending        22276 1995
    ## 221                Hawaii Total.Health.Spending         3972 1995
    ## 222                 Idaho Total.Health.Spending         3005 1995
    ## 223              Illinois Total.Health.Spending        40104 1995
    ## 224               Indiana Total.Health.Spending        18302 1995
    ## 225                  Iowa Total.Health.Spending         9085 1995
    ## 226                Kansas Total.Health.Spending         8367 1995
    ## 227              Kentucky Total.Health.Spending        11618 1995
    ## 228             Louisiana Total.Health.Spending        14578 1995
    ## 229                 Maine Total.Health.Spending         4052 1995
    ## 230              Maryland Total.Health.Spending        17273 1995
    ## 231         Massachusetts Total.Health.Spending        25226 1995
    ## 232              Michigan Total.Health.Spending        31842 1995
    ## 233             Minnesota Total.Health.Spending        15634 1995
    ## 234           Mississippi Total.Health.Spending         7761 1995
    ## 235              Missouri Total.Health.Spending        17099 1995
    ## 236               Montana Total.Health.Spending         2498 1995
    ## 237              Nebraska Total.Health.Spending         4901 1995
    ## 238                Nevada Total.Health.Spending         4404 1995
    ## 239         New Hampshire Total.Health.Spending         3642 1995
    ## 240            New Jersey Total.Health.Spending        30305 1995
    ## 241            New Mexico Total.Health.Spending         4713 1995
    ## 242              New York Total.Health.Spending        74482 1995
    ## 243        North Carolina Total.Health.Spending        21733 1995
    ## 244          North Dakota Total.Health.Spending         2261 1995
    ## 245                  Ohio Total.Health.Spending        37365 1995
    ## 246              Oklahoma Total.Health.Spending         9910 1995
    ## 247                Oregon Total.Health.Spending         9022 1995
    ## 248          Pennsylvania Total.Health.Spending        45388 1995
    ## 249          Rhode Island Total.Health.Spending         3812 1995
    ## 250        South Carolina Total.Health.Spending        10866 1995
    ## 251          South Dakota Total.Health.Spending         2269 1995
    ## 252             Tennessee Total.Health.Spending        17405 1995
    ## 253                 Texas Total.Health.Spending        54434 1995
    ## 254                  Utah Total.Health.Spending         4700 1995
    ## 255               Vermont Total.Health.Spending         1793 1995
    ## 256              Virginia Total.Health.Spending        18648 1995
    ## 257            Washington Total.Health.Spending        16515 1995
    ## 258         West Virginia Total.Health.Spending         6304 1995
    ## 259             Wisconsin Total.Health.Spending        16601 1995
    ## 260               Wyoming Total.Health.Spending         1378 1995
    ## 261         United States Total.Health.Spending       917540 1996
    ## 262               Alabama Total.Health.Spending        14450 1996
    ## 263                Alaska Total.Health.Spending         2076 1996
    ## 264               Arizona Total.Health.Spending        12850 1996
    ## 265              Arkansas Total.Health.Spending         7817 1996
    ## 266            California Total.Health.Spending       100215 1996
    ## 267              Colorado Total.Health.Spending        11863 1996
    ## 268           Connecticut Total.Health.Spending        14139 1996
    ## 269              Delaware Total.Health.Spending         2772 1996
    ## 270  District of Columbia Total.Health.Spending         3362 1996
    ## 271               Florida Total.Health.Spending        55090 1996
    ## 272               Georgia Total.Health.Spending        23622 1996
    ## 273                Hawaii Total.Health.Spending         4246 1996
    ## 274                 Idaho Total.Health.Spending         3260 1996
    ## 275              Illinois Total.Health.Spending        42069 1996
    ## 276               Indiana Total.Health.Spending        18747 1996
    ## 277                  Iowa Total.Health.Spending         9667 1996
    ## 278                Kansas Total.Health.Spending         8867 1996
    ## 279              Kentucky Total.Health.Spending        12473 1996
    ## 280             Louisiana Total.Health.Spending        15125 1996
    ## 281                 Maine Total.Health.Spending         4395 1996
    ## 282              Maryland Total.Health.Spending        18231 1996
    ## 283         Massachusetts Total.Health.Spending        26553 1996
    ## 284              Michigan Total.Health.Spending        33797 1996
    ## 285             Minnesota Total.Health.Spending        16559 1996
    ## 286           Mississippi Total.Health.Spending         8339 1996
    ## 287              Missouri Total.Health.Spending        18154 1996
    ## 288               Montana Total.Health.Spending         2577 1996
    ## 289              Nebraska Total.Health.Spending         5325 1996
    ## 290                Nevada Total.Health.Spending         4769 1996
    ## 291         New Hampshire Total.Health.Spending         3963 1996
    ## 292            New Jersey Total.Health.Spending        31420 1996
    ## 293            New Mexico Total.Health.Spending         5090 1996
    ## 294              New York Total.Health.Spending        78348 1996
    ## 295        North Carolina Total.Health.Spending        23678 1996
    ## 296          North Dakota Total.Health.Spending         2365 1996
    ## 297                  Ohio Total.Health.Spending        39749 1996
    ## 298              Oklahoma Total.Health.Spending        10590 1996
    ## 299                Oregon Total.Health.Spending         9530 1996
    ## 300          Pennsylvania Total.Health.Spending        46598 1996
    ## 301          Rhode Island Total.Health.Spending         3950 1996
    ## 302        South Carolina Total.Health.Spending        11487 1996
    ## 303          South Dakota Total.Health.Spending         2435 1996
    ## 304             Tennessee Total.Health.Spending        18603 1996
    ## 305                 Texas Total.Health.Spending        58352 1996
    ## 306                  Utah Total.Health.Spending         5153 1996
    ## 307               Vermont Total.Health.Spending         1894 1996
    ## 308              Virginia Total.Health.Spending        19958 1996
    ## 309            Washington Total.Health.Spending        17158 1996
    ## 310         West Virginia Total.Health.Spending         6632 1996
    ## 311             Wisconsin Total.Health.Spending        17702 1996
    ## 312               Wyoming Total.Health.Spending         1476 1996
    ## 313         United States Total.Health.Spending       969531 1997
    ## 314               Alabama Total.Health.Spending        15462 1997
    ## 315                Alaska Total.Health.Spending         2240 1997
    ## 316               Arizona Total.Health.Spending        13418 1997
    ## 317              Arkansas Total.Health.Spending         8393 1997
    ## 318            California Total.Health.Spending       103681 1997
    ## 319              Colorado Total.Health.Spending        12572 1997
    ## 320           Connecticut Total.Health.Spending        14948 1997
    ## 321              Delaware Total.Health.Spending         3026 1997
    ## 322  District of Columbia Total.Health.Spending         3374 1997
    ## 323               Florida Total.Health.Spending        58706 1997
    ## 324               Georgia Total.Health.Spending        24976 1997
    ## 325                Hawaii Total.Health.Spending         4263 1997
    ## 326                 Idaho Total.Health.Spending         3509 1997
    ## 327              Illinois Total.Health.Spending        44152 1997
    ## 328               Indiana Total.Health.Spending        20220 1997
    ## 329                  Iowa Total.Health.Spending        10183 1997
    ## 330                Kansas Total.Health.Spending         9428 1997
    ## 331              Kentucky Total.Health.Spending        13533 1997
    ## 332             Louisiana Total.Health.Spending        15920 1997
    ## 333                 Maine Total.Health.Spending         4752 1997
    ## 334              Maryland Total.Health.Spending        19093 1997
    ## 335         Massachusetts Total.Health.Spending        28098 1997
    ## 336              Michigan Total.Health.Spending        35562 1997
    ## 337             Minnesota Total.Health.Spending        17639 1997
    ## 338           Mississippi Total.Health.Spending         8947 1997
    ## 339              Missouri Total.Health.Spending        19263 1997
    ## 340               Montana Total.Health.Spending         2809 1997
    ## 341              Nebraska Total.Health.Spending         5621 1997
    ## 342                Nevada Total.Health.Spending         5211 1997
    ## 343         New Hampshire Total.Health.Spending         4195 1997
    ## 344            New Jersey Total.Health.Spending        33093 1997
    ## 345            New Mexico Total.Health.Spending         5436 1997
    ## 346              New York Total.Health.Spending        81674 1997
    ## 347        North Carolina Total.Health.Spending        25569 1997
    ## 348          North Dakota Total.Health.Spending         2449 1997
    ## 349                  Ohio Total.Health.Spending        40948 1997
    ## 350              Oklahoma Total.Health.Spending        11013 1997
    ## 351                Oregon Total.Health.Spending        10217 1997
    ## 352          Pennsylvania Total.Health.Spending        49227 1997
    ## 353          Rhode Island Total.Health.Spending         4186 1997
    ## 354        South Carolina Total.Health.Spending        12677 1997
    ## 355          South Dakota Total.Health.Spending         2595 1997
    ## 356             Tennessee Total.Health.Spending        19817 1997
    ## 357                 Texas Total.Health.Spending        63245 1997
    ## 358                  Utah Total.Health.Spending         5700 1997
    ## 359               Vermont Total.Health.Spending         2026 1997
    ## 360              Virginia Total.Health.Spending        21258 1997
    ## 361            Washington Total.Health.Spending        17978 1997
    ## 362         West Virginia Total.Health.Spending         7031 1997
    ## 363             Wisconsin Total.Health.Spending        18628 1997
    ## 364               Wyoming Total.Health.Spending         1568 1997
    ## 365         United States Total.Health.Spending      1026103 1998
    ## 366               Alabama Total.Health.Spending        15860 1998
    ## 367                Alaska Total.Health.Spending         2386 1998
    ## 368               Arizona Total.Health.Spending        14465 1998
    ## 369              Arkansas Total.Health.Spending         8814 1998
    ## 370            California Total.Health.Spending       111224 1998
    ## 371              Colorado Total.Health.Spending        13790 1998
    ## 372           Connecticut Total.Health.Spending        15944 1998
    ## 373              Delaware Total.Health.Spending         3207 1998
    ## 374  District of Columbia Total.Health.Spending         3461 1998
    ## 375               Florida Total.Health.Spending        61558 1998
    ## 376               Georgia Total.Health.Spending        26480 1998
    ## 377                Hawaii Total.Health.Spending         4446 1998
    ## 378                 Idaho Total.Health.Spending         3781 1998
    ## 379              Illinois Total.Health.Spending        46490 1998
    ## 380               Indiana Total.Health.Spending        21574 1998
    ## 381                  Iowa Total.Health.Spending        11057 1998
    ## 382                Kansas Total.Health.Spending        10071 1998
    ## 383              Kentucky Total.Health.Spending        14337 1998
    ## 384             Louisiana Total.Health.Spending        16532 1998
    ## 385                 Maine Total.Health.Spending         5151 1998
    ## 386              Maryland Total.Health.Spending        19538 1998
    ## 387         Massachusetts Total.Health.Spending        30165 1998
    ## 388              Michigan Total.Health.Spending        36417 1998
    ## 389             Minnesota Total.Health.Spending        19292 1998
    ## 390           Mississippi Total.Health.Spending         9334 1998
    ## 391              Missouri Total.Health.Spending        20574 1998
    ## 392               Montana Total.Health.Spending         2989 1998
    ## 393              Nebraska Total.Health.Spending         6026 1998
    ## 394                Nevada Total.Health.Spending         5694 1998
    ## 395         New Hampshire Total.Health.Spending         4545 1998
    ## 396            New Jersey Total.Health.Spending        35003 1998
    ## 397            New Mexico Total.Health.Spending         5689 1998
    ## 398              New York Total.Health.Spending        86599 1998
    ## 399        North Carolina Total.Health.Spending        26873 1998
    ## 400          North Dakota Total.Health.Spending         2605 1998
    ## 401                  Ohio Total.Health.Spending        42515 1998
    ## 402              Oklahoma Total.Health.Spending        11582 1998
    ## 403                Oregon Total.Health.Spending        11369 1998
    ## 404          Pennsylvania Total.Health.Spending        51278 1998
    ## 405          Rhode Island Total.Health.Spending         4460 1998
    ## 406        South Carolina Total.Health.Spending        13422 1998
    ## 407          South Dakota Total.Health.Spending         2728 1998
    ## 408             Tennessee Total.Health.Spending        20700 1998
    ## 409                 Texas Total.Health.Spending        67106 1998
    ## 410                  Utah Total.Health.Spending         5989 1998
    ## 411               Vermont Total.Health.Spending         2172 1998
    ## 412              Virginia Total.Health.Spending        22391 1998
    ## 413            Washington Total.Health.Spending        19746 1998
    ## 414         West Virginia Total.Health.Spending         7343 1998
    ## 415             Wisconsin Total.Health.Spending        19640 1998
    ## 416               Wyoming Total.Health.Spending         1690 1998
    ## 417         United States Total.Health.Spending      1086280 1999
    ## 418               Alabama Total.Health.Spending        16451 1999
    ## 419                Alaska Total.Health.Spending         2569 1999
    ## 420               Arizona Total.Health.Spending        15550 1999
    ## 421              Arkansas Total.Health.Spending         9407 1999
    ## 422            California Total.Health.Spending       116036 1999
    ## 423              Colorado Total.Health.Spending        14764 1999
    ## 424           Connecticut Total.Health.Spending        16785 1999
    ## 425              Delaware Total.Health.Spending         3539 1999
    ## 426  District of Columbia Total.Health.Spending         3578 1999
    ## 427               Florida Total.Health.Spending        65516 1999
    ## 428               Georgia Total.Health.Spending        27814 1999
    ## 429                Hawaii Total.Health.Spending         4570 1999
    ## 430                 Idaho Total.Health.Spending         4052 1999
    ## 431              Illinois Total.Health.Spending        48640 1999
    ## 432               Indiana Total.Health.Spending        22993 1999
    ## 433                  Iowa Total.Health.Spending        11740 1999
    ## 434                Kansas Total.Health.Spending        10723 1999
    ## 435              Kentucky Total.Health.Spending        15409 1999
    ## 436             Louisiana Total.Health.Spending        17437 1999
    ## 437                 Maine Total.Health.Spending         5604 1999
    ## 438              Maryland Total.Health.Spending        20986 1999
    ## 439         Massachusetts Total.Health.Spending        31012 1999
    ## 440              Michigan Total.Health.Spending        38058 1999
    ## 441             Minnesota Total.Health.Spending        20475 1999
    ## 442           Mississippi Total.Health.Spending         9714 1999
    ## 443              Missouri Total.Health.Spending        21727 1999
    ## 444               Montana Total.Health.Spending         3224 1999
    ## 445              Nebraska Total.Health.Spending         6461 1999
    ## 446                Nevada Total.Health.Spending         6249 1999
    ## 447         New Hampshire Total.Health.Spending         4907 1999
    ## 448            New Jersey Total.Health.Spending        36634 1999
    ## 449            New Mexico Total.Health.Spending         6003 1999
    ## 450              New York Total.Health.Spending        91675 1999
    ## 451        North Carolina Total.Health.Spending        29178 1999
    ## 452          North Dakota Total.Health.Spending         2683 1999
    ## 453                  Ohio Total.Health.Spending        44823 1999
    ## 454              Oklahoma Total.Health.Spending        12266 1999
    ## 455                Oregon Total.Health.Spending        12100 1999
    ## 456          Pennsylvania Total.Health.Spending        54667 1999
    ## 457          Rhode Island Total.Health.Spending         4696 1999
    ## 458        South Carolina Total.Health.Spending        14444 1999
    ## 459          South Dakota Total.Health.Spending         2967 1999
    ## 460             Tennessee Total.Health.Spending        21703 1999
    ## 461                 Texas Total.Health.Spending        71862 1999
    ## 462                  Utah Total.Health.Spending         6159 1999
    ## 463               Vermont Total.Health.Spending         2388 1999
    ## 464              Virginia Total.Health.Spending        23707 1999
    ## 465            Washington Total.Health.Spending        21504 1999
    ## 466         West Virginia Total.Health.Spending         7671 1999
    ## 467             Wisconsin Total.Health.Spending        21373 1999
    ## 468               Wyoming Total.Health.Spending         1783 1999
    ## 469         United States Total.Health.Spending      1162035 2000
    ## 470               Alabama Total.Health.Spending        17504 2000
    ## 471                Alaska Total.Health.Spending         2867 2000
    ## 472               Arizona Total.Health.Spending        16646 2000
    ## 473              Arkansas Total.Health.Spending        10009 2000
    ## 474            California Total.Health.Spending       121604 2000
    ## 475              Colorado Total.Health.Spending        16307 2000
    ## 476           Connecticut Total.Health.Spending        17691 2000
    ## 477              Delaware Total.Health.Spending         3760 2000
    ## 478  District of Columbia Total.Health.Spending         3611 2000
    ## 479               Florida Total.Health.Spending        71581 2000
    ## 480               Georgia Total.Health.Spending        29664 2000
    ## 481                Hawaii Total.Health.Spending         4712 2000
    ## 482                 Idaho Total.Health.Spending         4410 2000
    ## 483              Illinois Total.Health.Spending        52067 2000
    ## 484               Indiana Total.Health.Spending        24524 2000
    ## 485                  Iowa Total.Health.Spending        12459 2000
    ## 486                Kansas Total.Health.Spending        11441 2000
    ## 487              Kentucky Total.Health.Spending        16891 2000
    ## 488             Louisiana Total.Health.Spending        18101 2000
    ## 489                 Maine Total.Health.Spending         6021 2000
    ## 490              Maryland Total.Health.Spending        22511 2000
    ## 491         Massachusetts Total.Health.Spending        33080 2000
    ## 492              Michigan Total.Health.Spending        40287 2000
    ## 493             Minnesota Total.Health.Spending        22320 2000
    ## 494           Mississippi Total.Health.Spending        10618 2000
    ## 495              Missouri Total.Health.Spending        23420 2000
    ## 496               Montana Total.Health.Spending         3487 2000
    ## 497              Nebraska Total.Health.Spending         7101 2000
    ## 498                Nevada Total.Health.Spending         6961 2000
    ## 499         New Hampshire Total.Health.Spending         5264 2000
    ## 500            New Jersey Total.Health.Spending        39968 2000
    ## 501            New Mexico Total.Health.Spending         6300 2000
    ## 502              New York Total.Health.Spending        96634 2000
    ## 503        North Carolina Total.Health.Spending        31979 2000
    ## 504          North Dakota Total.Health.Spending         2837 2000
    ## 505                  Ohio Total.Health.Spending        48017 2000
    ## 506              Oklahoma Total.Health.Spending        13006 2000
    ## 507                Oregon Total.Health.Spending        12868 2000
    ## 508          Pennsylvania Total.Health.Spending        58166 2000
    ## 509          Rhode Island Total.Health.Spending         5052 2000
    ## 510        South Carolina Total.Health.Spending        15542 2000
    ## 511          South Dakota Total.Health.Spending         3145 2000
    ## 512             Tennessee Total.Health.Spending        23410 2000
    ## 513                 Texas Total.Health.Spending        76576 2000
    ## 514                  Utah Total.Health.Spending         6804 2000
    ## 515               Vermont Total.Health.Spending         2647 2000
    ## 516              Virginia Total.Health.Spending        26088 2000
    ## 517            Washington Total.Health.Spending        22874 2000
    ## 518         West Virginia Total.Health.Spending         8079 2000
    ## 519             Wisconsin Total.Health.Spending        23165 2000
    ## 520               Wyoming Total.Health.Spending         1958 2000
    ## 521         United States Total.Health.Spending      1261944 2001
    ## 522               Alabama Total.Health.Spending        18619 2001
    ## 523                Alaska Total.Health.Spending         3276 2001
    ## 524               Arizona Total.Health.Spending        18129 2001
    ## 525              Arkansas Total.Health.Spending        10846 2001
    ## 526            California Total.Health.Spending       132336 2001
    ## 527              Colorado Total.Health.Spending        17669 2001
    ## 528           Connecticut Total.Health.Spending        19087 2001
    ## 529              Delaware Total.Health.Spending         4051 2001
    ## 530  District of Columbia Total.Health.Spending         3828 2001
    ## 531               Florida Total.Health.Spending        77621 2001
    ## 532               Georgia Total.Health.Spending        31886 2001
    ## 533                Hawaii Total.Health.Spending         5031 2001
    ## 534                 Idaho Total.Health.Spending         4820 2001
    ## 535              Illinois Total.Health.Spending        55765 2001
    ## 536               Indiana Total.Health.Spending        26618 2001
    ## 537                  Iowa Total.Health.Spending        13411 2001
    ## 538                Kansas Total.Health.Spending        12312 2001
    ## 539              Kentucky Total.Health.Spending        18198 2001
    ## 540             Louisiana Total.Health.Spending        19207 2001
    ## 541                 Maine Total.Health.Spending         6595 2001
    ## 542              Maryland Total.Health.Spending        24744 2001
    ## 543         Massachusetts Total.Health.Spending        36008 2001
    ## 544              Michigan Total.Health.Spending        42827 2001
    ## 545             Minnesota Total.Health.Spending        24408 2001
    ## 546           Mississippi Total.Health.Spending        11660 2001
    ## 547              Missouri Total.Health.Spending        25201 2001
    ## 548               Montana Total.Health.Spending         3816 2001
    ## 549              Nebraska Total.Health.Spending         7846 2001
    ## 550                Nevada Total.Health.Spending         7800 2001
    ## 551         New Hampshire Total.Health.Spending         5678 2001
    ## 552            New Jersey Total.Health.Spending        42382 2001
    ## 553            New Mexico Total.Health.Spending         6809 2001
    ## 554              New York Total.Health.Spending       104574 2001
    ## 555        North Carolina Total.Health.Spending        35823 2001
    ## 556          North Dakota Total.Health.Spending         3091 2001
    ## 557                  Ohio Total.Health.Spending        53074 2001
    ## 558              Oklahoma Total.Health.Spending        14236 2001
    ## 559                Oregon Total.Health.Spending        14408 2001
    ## 560          Pennsylvania Total.Health.Spending        62608 2001
    ## 561          Rhode Island Total.Health.Spending         5485 2001
    ## 562        South Carolina Total.Health.Spending        17226 2001
    ## 563          South Dakota Total.Health.Spending         3319 2001
    ## 564             Tennessee Total.Health.Spending        25844 2001
    ## 565                 Texas Total.Health.Spending        84023 2001
    ## 566                  Utah Total.Health.Spending         7519 2001
    ## 567               Vermont Total.Health.Spending         2891 2001
    ## 568              Virginia Total.Health.Spending        27761 2001
    ## 569            Washington Total.Health.Spending        24968 2001
    ## 570         West Virginia Total.Health.Spending         8803 2001
    ## 571             Wisconsin Total.Health.Spending        25692 2001
    ## 572               Wyoming Total.Health.Spending         2112 2001
    ## 573         United States Total.Health.Spending      1367628 2002
    ## 574               Alabama Total.Health.Spending        20209 2002
    ## 575                Alaska Total.Health.Spending         3642 2002
    ## 576               Arizona Total.Health.Spending        20390 2002
    ## 577              Arkansas Total.Health.Spending        11797 2002
    ## 578            California Total.Health.Spending       143875 2002
    ## 579              Colorado Total.Health.Spending        19123 2002
    ## 580           Connecticut Total.Health.Spending        20558 2002
    ## 581              Delaware Total.Health.Spending         4462 2002
    ## 582  District of Columbia Total.Health.Spending         4118 2002
    ## 583               Florida Total.Health.Spending        83895 2002
    ## 584               Georgia Total.Health.Spending        35179 2002
    ## 585                Hawaii Total.Health.Spending         5463 2002
    ## 586                 Idaho Total.Health.Spending         5372 2002
    ## 587              Illinois Total.Health.Spending        60058 2002
    ## 588               Indiana Total.Health.Spending        28700 2002
    ## 589                  Iowa Total.Health.Spending        14215 2002
    ## 590                Kansas Total.Health.Spending        13167 2002
    ## 591              Kentucky Total.Health.Spending        19746 2002
    ## 592             Louisiana Total.Health.Spending        20513 2002
    ## 593                 Maine Total.Health.Spending         7231 2002
    ## 594              Maryland Total.Health.Spending        27120 2002
    ## 595         Massachusetts Total.Health.Spending        39320 2002
    ## 596              Michigan Total.Health.Spending        45585 2002
    ## 597             Minnesota Total.Health.Spending        26695 2002
    ## 598           Mississippi Total.Health.Spending        12621 2002
    ## 599              Missouri Total.Health.Spending        27551 2002
    ## 600               Montana Total.Health.Spending         4069 2002
    ## 601              Nebraska Total.Health.Spending         8422 2002
    ## 602                Nevada Total.Health.Spending         8899 2002
    ## 603         New Hampshire Total.Health.Spending         6114 2002
    ## 604            New Jersey Total.Health.Spending        46075 2002
    ## 605            New Mexico Total.Health.Spending         7406 2002
    ## 606              New York Total.Health.Spending       112615 2002
    ## 607        North Carolina Total.Health.Spending        38117 2002
    ## 608          North Dakota Total.Health.Spending         3398 2002
    ## 609                  Ohio Total.Health.Spending        57585 2002
    ## 610              Oklahoma Total.Health.Spending        15214 2002
    ## 611                Oregon Total.Health.Spending        15455 2002
    ## 612          Pennsylvania Total.Health.Spending        66745 2002
    ## 613          Rhode Island Total.Health.Spending         5977 2002
    ## 614        South Carolina Total.Health.Spending        18686 2002
    ## 615          South Dakota Total.Health.Spending         3674 2002
    ## 616             Tennessee Total.Health.Spending        27251 2002
    ## 617                 Texas Total.Health.Spending        92317 2002
    ## 618                  Utah Total.Health.Spending         8392 2002
    ## 619               Vermont Total.Health.Spending         3181 2002
    ## 620              Virginia Total.Health.Spending        30500 2002
    ## 621            Washington Total.Health.Spending        27404 2002
    ## 622         West Virginia Total.Health.Spending         9494 2002
    ## 623             Wisconsin Total.Health.Spending        27691 2002
    ## 624               Wyoming Total.Health.Spending         2346 2002
    ## 625         United States Total.Health.Spending      1477697 2003
    ## 626               Alabama Total.Health.Spending        22491 2003
    ## 627                Alaska Total.Health.Spending         3955 2003
    ## 628               Arizona Total.Health.Spending        22464 2003
    ## 629              Arkansas Total.Health.Spending        12578 2003
    ## 630            California Total.Health.Spending       158268 2003
    ## 631              Colorado Total.Health.Spending        20690 2003
    ## 632           Connecticut Total.Health.Spending        21695 2003
    ## 633              Delaware Total.Health.Spending         5042 2003
    ## 634  District of Columbia Total.Health.Spending         4355 2003
    ## 635               Florida Total.Health.Spending        90733 2003
    ## 636               Georgia Total.Health.Spending        37579 2003
    ## 637                Hawaii Total.Health.Spending         5943 2003
    ## 638                 Idaho Total.Health.Spending         5798 2003
    ## 639              Illinois Total.Health.Spending        63790 2003
    ## 640               Indiana Total.Health.Spending        31157 2003
    ## 641                  Iowa Total.Health.Spending        15331 2003
    ## 642                Kansas Total.Health.Spending        14021 2003
    ## 643              Kentucky Total.Health.Spending        20941 2003
    ## 644             Louisiana Total.Health.Spending        22229 2003
    ## 645                 Maine Total.Health.Spending         7952 2003
    ## 646              Maryland Total.Health.Spending        29572 2003
    ## 647         Massachusetts Total.Health.Spending        42429 2003
    ## 648              Michigan Total.Health.Spending        48870 2003
    ## 649             Minnesota Total.Health.Spending        28765 2003
    ## 650           Mississippi Total.Health.Spending        13505 2003
    ## 651              Missouri Total.Health.Spending        29647 2003
    ## 652               Montana Total.Health.Spending         4409 2003
    ## 653              Nebraska Total.Health.Spending         9080 2003
    ## 654                Nevada Total.Health.Spending         9891 2003
    ## 655         New Hampshire Total.Health.Spending         6690 2003
    ## 656            New Jersey Total.Health.Spending        48847 2003
    ## 657            New Mexico Total.Health.Spending         7920 2003
    ## 658              New York Total.Health.Spending       122495 2003
    ## 659        North Carolina Total.Health.Spending        41272 2003
    ## 660          North Dakota Total.Health.Spending         3529 2003
    ## 661                  Ohio Total.Health.Spending        62078 2003
    ## 662              Oklahoma Total.Health.Spending        16653 2003
    ## 663                Oregon Total.Health.Spending        16590 2003
    ## 664          Pennsylvania Total.Health.Spending        71938 2003
    ## 665          Rhode Island Total.Health.Spending         6456 2003
    ## 666        South Carolina Total.Health.Spending        20063 2003
    ## 667          South Dakota Total.Health.Spending         3956 2003
    ## 668             Tennessee Total.Health.Spending        29781 2003
    ## 669                 Texas Total.Health.Spending        98066 2003
    ## 670                  Utah Total.Health.Spending         9109 2003
    ## 671               Vermont Total.Health.Spending         3546 2003
    ## 672              Virginia Total.Health.Spending        33439 2003
    ## 673            Washington Total.Health.Spending        29710 2003
    ## 674         West Virginia Total.Health.Spending        10227 2003
    ## 675             Wisconsin Total.Health.Spending        29639 2003
    ## 676               Wyoming Total.Health.Spending         2512 2003
    ## 677         United States Total.Health.Spending      1587994 2004
    ## 678               Alabama Total.Health.Spending        23797 2004
    ## 679                Alaska Total.Health.Spending         4256 2004
    ## 680               Arizona Total.Health.Spending        24795 2004
    ## 681              Arkansas Total.Health.Spending        13470 2004
    ## 682            California Total.Health.Spending       170083 2004
    ## 683              Colorado Total.Health.Spending        22103 2004
    ## 684           Connecticut Total.Health.Spending        23560 2004
    ## 685              Delaware Total.Health.Spending         5498 2004
    ## 686  District of Columbia Total.Health.Spending         4766 2004
    ## 687               Florida Total.Health.Spending        99100 2004
    ## 688               Georgia Total.Health.Spending        41499 2004
    ## 689                Hawaii Total.Health.Spending         6391 2004
    ## 690                 Idaho Total.Health.Spending         6252 2004
    ## 691              Illinois Total.Health.Spending        67505 2004
    ## 692               Indiana Total.Health.Spending        33705 2004
    ## 693                  Iowa Total.Health.Spending        16203 2004
    ## 694                Kansas Total.Health.Spending        15008 2004
    ## 695              Kentucky Total.Health.Spending        22410 2004
    ## 696             Louisiana Total.Health.Spending        23953 2004
    ## 697                 Maine Total.Health.Spending         8644 2004
    ## 698              Maryland Total.Health.Spending        31821 2004
    ## 699         Massachusetts Total.Health.Spending        45069 2004
    ## 700              Michigan Total.Health.Spending        52684 2004
    ## 701             Minnesota Total.Health.Spending        30654 2004
    ## 702           Mississippi Total.Health.Spending        14708 2004
    ## 703              Missouri Total.Health.Spending        31341 2004
    ## 704               Montana Total.Health.Spending         4706 2004
    ## 705              Nebraska Total.Health.Spending         9789 2004
    ## 706                Nevada Total.Health.Spending        11029 2004
    ## 707         New Hampshire Total.Health.Spending         7395 2004
    ## 708            New Jersey Total.Health.Spending        51857 2004
    ## 709            New Mexico Total.Health.Spending         8764 2004
    ## 710              New York Total.Health.Spending       130033 2004
    ## 711        North Carolina Total.Health.Spending        45033 2004
    ## 712          North Dakota Total.Health.Spending         3791 2004
    ## 713                  Ohio Total.Health.Spending        66341 2004
    ## 714              Oklahoma Total.Health.Spending        17525 2004
    ## 715                Oregon Total.Health.Spending        17967 2004
    ## 716          Pennsylvania Total.Health.Spending        76830 2004
    ## 717          Rhode Island Total.Health.Spending         6982 2004
    ## 718        South Carolina Total.Health.Spending        21635 2004
    ## 719          South Dakota Total.Health.Spending         4180 2004
    ## 720             Tennessee Total.Health.Spending        32645 2004
    ## 721                 Texas Total.Health.Spending       104784 2004
    ## 722                  Utah Total.Health.Spending         9951 2004
    ## 723               Vermont Total.Health.Spending         3803 2004
    ## 724              Virginia Total.Health.Spending        36469 2004
    ## 725            Washington Total.Health.Spending        31678 2004
    ## 726         West Virginia Total.Health.Spending        10960 2004
    ## 727             Wisconsin Total.Health.Spending        31881 2004
    ## 728               Wyoming Total.Health.Spending         2692 2004
    ## 729         United States Total.Health.Spending      1696222 2005
    ## 730               Alabama Total.Health.Spending        25338 2005
    ## 731                Alaska Total.Health.Spending         4765 2005
    ## 732               Arizona Total.Health.Spending        28190 2005
    ## 733              Arkansas Total.Health.Spending        14611 2005
    ## 734            California Total.Health.Spending       182958 2005
    ## 735              Colorado Total.Health.Spending        22867 2005
    ## 736           Connecticut Total.Health.Spending        24538 2005
    ## 737              Delaware Total.Health.Spending         5899 2005
    ## 738  District of Columbia Total.Health.Spending         4971 2005
    ## 739               Florida Total.Health.Spending       106293 2005
    ## 740               Georgia Total.Health.Spending        43655 2005
    ## 741                Hawaii Total.Health.Spending         6967 2005
    ## 742                 Idaho Total.Health.Spending         6854 2005
    ## 743              Illinois Total.Health.Spending        71747 2005
    ## 744               Indiana Total.Health.Spending        35391 2005
    ## 745                  Iowa Total.Health.Spending        17241 2005
    ## 746                Kansas Total.Health.Spending        15885 2005
    ## 747              Kentucky Total.Health.Spending        23709 2005
    ## 748             Louisiana Total.Health.Spending        24866 2005
    ## 749                 Maine Total.Health.Spending         9233 2005
    ## 750              Maryland Total.Health.Spending        33911 2005
    ## 751         Massachusetts Total.Health.Spending        47920 2005
    ## 752              Michigan Total.Health.Spending        55564 2005
    ## 753             Minnesota Total.Health.Spending        32417 2005
    ## 754           Mississippi Total.Health.Spending        15653 2005
    ## 755              Missouri Total.Health.Spending        32853 2005
    ## 756               Montana Total.Health.Spending         5107 2005
    ## 757              Nebraska Total.Health.Spending        10491 2005
    ## 758                Nevada Total.Health.Spending        11995 2005
    ## 759         New Hampshire Total.Health.Spending         8143 2005
    ## 760            New Jersey Total.Health.Spending        55670 2005
    ## 761            New Mexico Total.Health.Spending         9702 2005
    ## 762              New York Total.Health.Spending       135771 2005
    ## 763        North Carolina Total.Health.Spending        48395 2005
    ## 764          North Dakota Total.Health.Spending         4009 2005
    ## 765                  Ohio Total.Health.Spending        70218 2005
    ## 766              Oklahoma Total.Health.Spending        19040 2005
    ## 767                Oregon Total.Health.Spending        19495 2005
    ## 768          Pennsylvania Total.Health.Spending        82123 2005
    ## 769          Rhode Island Total.Health.Spending         7497 2005
    ## 770        South Carolina Total.Health.Spending        23070 2005
    ## 771          South Dakota Total.Health.Spending         4564 2005
    ## 772             Tennessee Total.Health.Spending        34589 2005
    ## 773                 Texas Total.Health.Spending       115264 2005
    ## 774                  Utah Total.Health.Spending        10812 2005
    ## 775               Vermont Total.Health.Spending         4080 2005
    ## 776              Virginia Total.Health.Spending        39148 2005
    ## 777            Washington Total.Health.Spending        34101 2005
    ## 778         West Virginia Total.Health.Spending        11643 2005
    ## 779             Wisconsin Total.Health.Spending        34078 2005
    ## 780               Wyoming Total.Health.Spending         2922 2005
    ## 781         United States Total.Health.Spending      1804672 2006
    ## 782               Alabama Total.Health.Spending        26638 2006
    ## 783                Alaska Total.Health.Spending         5048 2006
    ## 784               Arizona Total.Health.Spending        30766 2006
    ## 785              Arkansas Total.Health.Spending        15431 2006
    ## 786            California Total.Health.Spending       194413 2006
    ## 787              Colorado Total.Health.Spending        24849 2006
    ## 788           Connecticut Total.Health.Spending        25997 2006
    ## 789              Delaware Total.Health.Spending         6285 2006
    ## 790  District of Columbia Total.Health.Spending         5138 2006
    ## 791               Florida Total.Health.Spending       114162 2006
    ## 792               Georgia Total.Health.Spending        47131 2006
    ## 793                Hawaii Total.Health.Spending         7275 2006
    ## 794                 Idaho Total.Health.Spending         7426 2006
    ## 795              Illinois Total.Health.Spending        76296 2006
    ## 796               Indiana Total.Health.Spending        37908 2006
    ## 797                  Iowa Total.Health.Spending        18411 2006
    ## 798                Kansas Total.Health.Spending        16689 2006
    ## 799              Kentucky Total.Health.Spending        25217 2006
    ## 800             Louisiana Total.Health.Spending        25573 2006
    ## 801                 Maine Total.Health.Spending         9760 2006
    ## 802              Maryland Total.Health.Spending        36747 2006
    ## 803         Massachusetts Total.Health.Spending        51637 2006
    ## 804              Michigan Total.Health.Spending        59457 2006
    ## 805             Minnesota Total.Health.Spending        34407 2006
    ## 806           Mississippi Total.Health.Spending        16826 2006
    ## 807              Missouri Total.Health.Spending        33959 2006
    ## 808               Montana Total.Health.Spending         5472 2006
    ## 809              Nebraska Total.Health.Spending        11300 2006
    ## 810                Nevada Total.Health.Spending        12991 2006
    ## 811         New Hampshire Total.Health.Spending         8809 2006
    ## 812            New Jersey Total.Health.Spending        58264 2006
    ## 813            New Mexico Total.Health.Spending        10316 2006
    ## 814              New York Total.Health.Spending       143635 2006
    ## 815        North Carolina Total.Health.Spending        52192 2006
    ## 816          North Dakota Total.Health.Spending         4269 2006
    ## 817                  Ohio Total.Health.Spending        73333 2006
    ## 818              Oklahoma Total.Health.Spending        20416 2006
    ## 819                Oregon Total.Health.Spending        21077 2006
    ## 820          Pennsylvania Total.Health.Spending        86069 2006
    ## 821          Rhode Island Total.Health.Spending         7815 2006
    ## 822        South Carolina Total.Health.Spending        24640 2006
    ## 823          South Dakota Total.Health.Spending         4900 2006
    ## 824             Tennessee Total.Health.Spending        36229 2006
    ## 825                 Texas Total.Health.Spending       123253 2006
    ## 826                  Utah Total.Health.Spending        11570 2006
    ## 827               Vermont Total.Health.Spending         4319 2006
    ## 828              Virginia Total.Health.Spending        41911 2006
    ## 829            Washington Total.Health.Spending        36500 2006
    ## 830         West Virginia Total.Health.Spending        12258 2006
    ## 831             Wisconsin Total.Health.Spending        36475 2006
    ## 832               Wyoming Total.Health.Spending         3212 2006
    ## 833         United States Total.Health.Spending      1918820 2007
    ## 834               Alabama Total.Health.Spending        27700 2007
    ## 835                Alaska Total.Health.Spending         5426 2007
    ## 836               Arizona Total.Health.Spending        33366 2007
    ## 837              Arkansas Total.Health.Spending        16426 2007
    ## 838            California Total.Health.Spending       209397 2007
    ## 839              Colorado Total.Health.Spending        26525 2007
    ## 840           Connecticut Total.Health.Spending        27488 2007
    ## 841              Delaware Total.Health.Spending         6735 2007
    ## 842  District of Columbia Total.Health.Spending         5492 2007
    ## 843               Florida Total.Health.Spending       120960 2007
    ## 844               Georgia Total.Health.Spending        49827 2007
    ## 845                Hawaii Total.Health.Spending         8023 2007
    ## 846                 Idaho Total.Health.Spending         7990 2007
    ## 847              Illinois Total.Health.Spending        81934 2007
    ## 848               Indiana Total.Health.Spending        40055 2007
    ## 849                  Iowa Total.Health.Spending        19327 2007
    ## 850                Kansas Total.Health.Spending        17684 2007
    ## 851              Kentucky Total.Health.Spending        26396 2007
    ## 852             Louisiana Total.Health.Spending        27532 2007
    ## 853                 Maine Total.Health.Spending        10243 2007
    ## 854              Maryland Total.Health.Spending        39047 2007
    ## 855         Massachusetts Total.Health.Spending        55554 2007
    ## 856              Michigan Total.Health.Spending        62551 2007
    ## 857             Minnesota Total.Health.Spending        36648 2007
    ## 858           Mississippi Total.Health.Spending        18083 2007
    ## 859              Missouri Total.Health.Spending        37229 2007
    ## 860               Montana Total.Health.Spending         5868 2007
    ## 861              Nebraska Total.Health.Spending        11883 2007
    ## 862                Nevada Total.Health.Spending        14123 2007
    ## 863         New Hampshire Total.Health.Spending         9442 2007
    ## 864            New Jersey Total.Health.Spending        61598 2007
    ## 865            New Mexico Total.Health.Spending        11089 2007
    ## 866              New York Total.Health.Spending       150601 2007
    ## 867        North Carolina Total.Health.Spending        54832 2007
    ## 868          North Dakota Total.Health.Spending         4576 2007
    ## 869                  Ohio Total.Health.Spending        76591 2007
    ## 870              Oklahoma Total.Health.Spending        21924 2007
    ## 871                Oregon Total.Health.Spending        22264 2007
    ## 872          Pennsylvania Total.Health.Spending        91040 2007
    ## 873          Rhode Island Total.Health.Spending         8196 2007
    ## 874        South Carolina Total.Health.Spending        26169 2007
    ## 875          South Dakota Total.Health.Spending         5208 2007
    ## 876             Tennessee Total.Health.Spending        38407 2007
    ## 877                 Texas Total.Health.Spending       130965 2007
    ## 878                  Utah Total.Health.Spending        12466 2007
    ## 879               Vermont Total.Health.Spending         4548 2007
    ## 880              Virginia Total.Health.Spending        44769 2007
    ## 881            Washington Total.Health.Spending        39595 2007
    ## 882         West Virginia Total.Health.Spending        12996 2007
    ## 883             Wisconsin Total.Health.Spending        38529 2007
    ## 884               Wyoming Total.Health.Spending         3501 2007
    ## 885         United States Total.Health.Spending      2010690 2008
    ## 886               Alabama Total.Health.Spending        28765 2008
    ## 887                Alaska Total.Health.Spending         5807 2008
    ## 888               Arizona Total.Health.Spending        35547 2008
    ## 889              Arkansas Total.Health.Spending        17246 2008
    ## 890            California Total.Health.Spending       221013 2008
    ## 891              Colorado Total.Health.Spending        27797 2008
    ## 892           Connecticut Total.Health.Spending        29141 2008
    ## 893              Delaware Total.Health.Spending         7191 2008
    ## 894  District of Columbia Total.Health.Spending         5779 2008
    ## 895               Florida Total.Health.Spending       126977 2008
    ## 896               Georgia Total.Health.Spending        50930 2008
    ## 897                Hawaii Total.Health.Spending         8503 2008
    ## 898                 Idaho Total.Health.Spending         8377 2008
    ## 899              Illinois Total.Health.Spending        84809 2008
    ## 900               Indiana Total.Health.Spending        41200 2008
    ## 901                  Iowa Total.Health.Spending        20184 2008
    ## 902                Kansas Total.Health.Spending        18502 2008
    ## 903              Kentucky Total.Health.Spending        27282 2008
    ## 904             Louisiana Total.Health.Spending        29353 2008
    ## 905                 Maine Total.Health.Spending        10725 2008
    ## 906              Maryland Total.Health.Spending        40931 2008
    ## 907         Massachusetts Total.Health.Spending        58260 2008
    ## 908              Michigan Total.Health.Spending        64824 2008
    ## 909             Minnesota Total.Health.Spending        37934 2008
    ## 910           Mississippi Total.Health.Spending        18931 2008
    ## 911              Missouri Total.Health.Spending        39643 2008
    ## 912               Montana Total.Health.Spending         6223 2008
    ## 913              Nebraska Total.Health.Spending        12540 2008
    ## 914                Nevada Total.Health.Spending        14907 2008
    ## 915         New Hampshire Total.Health.Spending         9967 2008
    ## 916            New Jersey Total.Health.Spending        63759 2008
    ## 917            New Mexico Total.Health.Spending        11896 2008
    ## 918              New York Total.Health.Spending       155573 2008
    ## 919        North Carolina Total.Health.Spending        57844 2008
    ## 920          North Dakota Total.Health.Spending         4892 2008
    ## 921                  Ohio Total.Health.Spending        78831 2008
    ## 922              Oklahoma Total.Health.Spending        23357 2008
    ## 923                Oregon Total.Health.Spending        23390 2008
    ## 924          Pennsylvania Total.Health.Spending        94455 2008
    ## 925          Rhode Island Total.Health.Spending         8523 2008
    ## 926        South Carolina Total.Health.Spending        27894 2008
    ## 927          South Dakota Total.Health.Spending         5556 2008
    ## 928             Tennessee Total.Health.Spending        39387 2008
    ## 929                 Texas Total.Health.Spending       139968 2008
    ## 930                  Utah Total.Health.Spending        13287 2008
    ## 931               Vermont Total.Health.Spending         4758 2008
    ## 932              Virginia Total.Health.Spending        47819 2008
    ## 933            Washington Total.Health.Spending        42569 2008
    ## 934         West Virginia Total.Health.Spending        13482 2008
    ## 935             Wisconsin Total.Health.Spending        40442 2008
    ## 936               Wyoming Total.Health.Spending         3719 2008
    ## 937         United States Total.Health.Spending      2114221 2009
    ## 938               Alabama Total.Health.Spending        30095 2009
    ## 939                Alaska Total.Health.Spending         6112 2009
    ## 940               Arizona Total.Health.Spending        37258 2009
    ## 941              Arkansas Total.Health.Spending        18071 2009
    ## 942            California Total.Health.Spending       229541 2009
    ## 943              Colorado Total.Health.Spending        29246 2009
    ## 944           Connecticut Total.Health.Spending        31132 2009
    ## 945              Delaware Total.Health.Spending         7495 2009
    ## 946  District of Columbia Total.Health.Spending         6182 2009
    ## 947               Florida Total.Health.Spending       133067 2009
    ## 948               Georgia Total.Health.Spending        53035 2009
    ## 949                Hawaii Total.Health.Spending         8810 2009
    ## 950                 Idaho Total.Health.Spending         8861 2009
    ## 951              Illinois Total.Health.Spending        88515 2009
    ## 952               Indiana Total.Health.Spending        43867 2009
    ## 953                  Iowa Total.Health.Spending        21067 2009
    ## 954                Kansas Total.Health.Spending        19161 2009
    ## 955              Kentucky Total.Health.Spending        28917 2009
    ## 956             Louisiana Total.Health.Spending        31253 2009
    ## 957                 Maine Total.Health.Spending        11114 2009
    ## 958              Maryland Total.Health.Spending        43019 2009
    ## 959         Massachusetts Total.Health.Spending        61376 2009
    ## 960              Michigan Total.Health.Spending        67489 2009
    ## 961             Minnesota Total.Health.Spending        39720 2009
    ## 962           Mississippi Total.Health.Spending        19572 2009
    ## 963              Missouri Total.Health.Spending        41141 2009
    ## 964               Montana Total.Health.Spending         6594 2009
    ## 965              Nebraska Total.Health.Spending        13038 2009
    ## 966                Nevada Total.Health.Spending        15302 2009
    ## 967         New Hampshire Total.Health.Spending        10705 2009
    ## 968            New Jersey Total.Health.Spending        67653 2009
    ## 969            New Mexico Total.Health.Spending        12657 2009
    ## 970              New York Total.Health.Spending       164928 2009
    ## 971        North Carolina Total.Health.Spending        61730 2009
    ## 972          North Dakota Total.Health.Spending         5266 2009
    ## 973                  Ohio Total.Health.Spending        84413 2009
    ## 974              Oklahoma Total.Health.Spending        24179 2009
    ## 975                Oregon Total.Health.Spending        24697 2009
    ## 976          Pennsylvania Total.Health.Spending        97553 2009
    ## 977          Rhode Island Total.Health.Spending         8843 2009
    ## 978        South Carolina Total.Health.Spending        29204 2009
    ## 979          South Dakota Total.Health.Spending         5919 2009
    ## 980             Tennessee Total.Health.Spending        40982 2009
    ## 981                 Texas Total.Health.Spending       148903 2009
    ## 982                  Utah Total.Health.Spending        13893 2009
    ## 983               Vermont Total.Health.Spending         5068 2009
    ## 984              Virginia Total.Health.Spending        51141 2009
    ## 985            Washington Total.Health.Spending        45589 2009
    ## 986         West Virginia Total.Health.Spending        14360 2009
    ## 987             Wisconsin Total.Health.Spending        42586 2009
    ## 988               Wyoming Total.Health.Spending         3903 2009
    ## 989         United States Total.Health.Spending      2194625 2010
    ## 990               Alabama Total.Health.Spending        30728 2010
    ## 991                Alaska Total.Health.Spending         6519 2010
    ## 992               Arizona Total.Health.Spending        38620 2010
    ## 993              Arkansas Total.Health.Spending        18735 2010
    ## 994            California Total.Health.Spending       241916 2010
    ## 995              Colorado Total.Health.Spending        30187 2010
    ## 996           Connecticut Total.Health.Spending        31727 2010
    ## 997              Delaware Total.Health.Spending         7938 2010
    ## 998  District of Columbia Total.Health.Spending         6582 2010
    ## 999               Florida Total.Health.Spending       137609 2010
    ## 1000              Georgia Total.Health.Spending        53950 2010
    ## 1001               Hawaii Total.Health.Spending         8860 2010
    ## 1002                Idaho Total.Health.Spending         9424 2010
    ## 1003             Illinois Total.Health.Spending        93140 2010
    ## 1004              Indiana Total.Health.Spending        45352 2010
    ## 1005                 Iowa Total.Health.Spending        21895 2010
    ## 1006               Kansas Total.Health.Spending        19605 2010
    ## 1007             Kentucky Total.Health.Spending        29995 2010
    ## 1008            Louisiana Total.Health.Spending        32848 2010
    ## 1009                Maine Total.Health.Spending        11338 2010
    ## 1010             Maryland Total.Health.Spending        44847 2010
    ## 1011        Massachusetts Total.Health.Spending        63153 2010
    ## 1012             Michigan Total.Health.Spending        70336 2010
    ## 1013            Minnesota Total.Health.Spending        41333 2010
    ## 1014          Mississippi Total.Health.Spending        19729 2010
    ## 1015             Missouri Total.Health.Spending        42659 2010
    ## 1016              Montana Total.Health.Spending         6968 2010
    ## 1017             Nebraska Total.Health.Spending        13769 2010
    ## 1018               Nevada Total.Health.Spending        15652 2010
    ## 1019        New Hampshire Total.Health.Spending        11149 2010
    ## 1020           New Jersey Total.Health.Spending        68477 2010
    ## 1021           New Mexico Total.Health.Spending        13294 2010
    ## 1022             New York Total.Health.Spending       170644 2010
    ## 1023       North Carolina Total.Health.Spending        63235 2010
    ## 1024         North Dakota Total.Health.Spending         5615 2010
    ## 1025                 Ohio Total.Health.Spending        86841 2010
    ## 1026             Oklahoma Total.Health.Spending        24993 2010
    ## 1027               Oregon Total.Health.Spending        25824 2010
    ## 1028         Pennsylvania Total.Health.Spending       103235 2010
    ## 1029         Rhode Island Total.Health.Spending         9026 2010
    ## 1030       South Carolina Total.Health.Spending        30382 2010
    ## 1031         South Dakota Total.Health.Spending         6289 2010
    ## 1032            Tennessee Total.Health.Spending        42117 2010
    ## 1033                Texas Total.Health.Spending       155553 2010
    ## 1034                 Utah Total.Health.Spending        14350 2010
    ## 1035              Vermont Total.Health.Spending         5314 2010
    ## 1036             Virginia Total.Health.Spending        53048 2010
    ## 1037           Washington Total.Health.Spending        47077 2010
    ## 1038        West Virginia Total.Health.Spending        14760 2010
    ## 1039            Wisconsin Total.Health.Spending        43867 2010
    ## 1040              Wyoming Total.Health.Spending         4121 2010
    ## 1041        United States Total.Health.Spending      2272582 2011
    ## 1042              Alabama Total.Health.Spending        31398 2011
    ## 1043               Alaska Total.Health.Spending         6928 2011
    ## 1044              Arizona Total.Health.Spending        39295 2011
    ## 1045             Arkansas Total.Health.Spending        19356 2011
    ## 1046           California Total.Health.Spending       253844 2011
    ## 1047             Colorado Total.Health.Spending        31372 2011
    ## 1048          Connecticut Total.Health.Spending        32129 2011
    ## 1049             Delaware Total.Health.Spending         8365 2011
    ## 1050 District of Columbia Total.Health.Spending         7000 2011
    ## 1051              Florida Total.Health.Spending       141462 2011
    ## 1052              Georgia Total.Health.Spending        56053 2011
    ## 1053               Hawaii Total.Health.Spending         8993 2011
    ## 1054                Idaho Total.Health.Spending         9718 2011
    ## 1055             Illinois Total.Health.Spending        95542 2011
    ## 1056              Indiana Total.Health.Spending        47476 2011
    ## 1057                 Iowa Total.Health.Spending        22733 2011
    ## 1058               Kansas Total.Health.Spending        20522 2011
    ## 1059             Kentucky Total.Health.Spending        31206 2011
    ## 1060            Louisiana Total.Health.Spending        32765 2011
    ## 1061                Maine Total.Health.Spending        11720 2011
    ## 1062             Maryland Total.Health.Spending        46381 2011
    ## 1063        Massachusetts Total.Health.Spending        64915 2011
    ## 1064             Michigan Total.Health.Spending        73143 2011
    ## 1065            Minnesota Total.Health.Spending        42618 2011
    ## 1066          Mississippi Total.Health.Spending        20351 2011
    ## 1067             Missouri Total.Health.Spending        44726 2011
    ## 1068              Montana Total.Health.Spending         7285 2011
    ## 1069             Nebraska Total.Health.Spending        14213 2011
    ## 1070               Nevada Total.Health.Spending        16198 2011
    ## 1071        New Hampshire Total.Health.Spending        11558 2011
    ## 1072           New Jersey Total.Health.Spending        70263 2011
    ## 1073           New Mexico Total.Health.Spending        13644 2011
    ## 1074             New York Total.Health.Spending       175996 2011
    ## 1075       North Carolina Total.Health.Spending        65703 2011
    ## 1076         North Dakota Total.Health.Spending         6003 2011
    ## 1077                 Ohio Total.Health.Spending        88339 2011
    ## 1078             Oklahoma Total.Health.Spending        26205 2011
    ## 1079               Oregon Total.Health.Spending        26964 2011
    ## 1080         Pennsylvania Total.Health.Spending       107458 2011
    ## 1081         Rhode Island Total.Health.Spending         9243 2011
    ## 1082       South Carolina Total.Health.Spending        31339 2011
    ## 1083         South Dakota Total.Health.Spending         6649 2011
    ## 1084            Tennessee Total.Health.Spending        43134 2011
    ## 1085                Texas Total.Health.Spending       162296 2011
    ## 1086                 Utah Total.Health.Spending        15042 2011
    ## 1087              Vermont Total.Health.Spending         5562 2011
    ## 1088             Virginia Total.Health.Spending        55772 2011
    ## 1089           Washington Total.Health.Spending        48567 2011
    ## 1090        West Virginia Total.Health.Spending        15336 2011
    ## 1091            Wisconsin Total.Health.Spending        45511 2011
    ## 1092              Wyoming Total.Health.Spending         4289 2011
    ## 1093        United States Total.Health.Spending      2365948 2012
    ## 1094              Alabama Total.Health.Spending        32848 2012
    ## 1095               Alaska Total.Health.Spending         7406 2012
    ## 1096              Arizona Total.Health.Spending        40495 2012
    ## 1097             Arkansas Total.Health.Spending        20076 2012
    ## 1098           California Total.Health.Spending       266767 2012
    ## 1099             Colorado Total.Health.Spending        32726 2012
    ## 1100          Connecticut Total.Health.Spending        33421 2012
    ## 1101             Delaware Total.Health.Spending         8650 2012
    ## 1102 District of Columbia Total.Health.Spending         7130 2012
    ## 1103              Florida Total.Health.Spending       147685 2012
    ## 1104              Georgia Total.Health.Spending        59575 2012
    ## 1105               Hawaii Total.Health.Spending         9469 2012
    ## 1106                Idaho Total.Health.Spending        10182 2012
    ## 1107             Illinois Total.Health.Spending        98651 2012
    ## 1108              Indiana Total.Health.Spending        51306 2012
    ## 1109                 Iowa Total.Health.Spending        23528 2012
    ## 1110               Kansas Total.Health.Spending        21573 2012
    ## 1111             Kentucky Total.Health.Spending        31960 2012
    ## 1112            Louisiana Total.Health.Spending        33618 2012
    ## 1113                Maine Total.Health.Spending        11961 2012
    ## 1114             Maryland Total.Health.Spending        47792 2012
    ## 1115        Massachusetts Total.Health.Spending        67055 2012
    ## 1116             Michigan Total.Health.Spending        75507 2012
    ## 1117            Minnesota Total.Health.Spending        43997 2012
    ## 1118          Mississippi Total.Health.Spending        21995 2012
    ## 1119             Missouri Total.Health.Spending        46743 2012
    ## 1120              Montana Total.Health.Spending         7685 2012
    ## 1121             Nebraska Total.Health.Spending        14808 2012
    ## 1122               Nevada Total.Health.Spending        16588 2012
    ## 1123        New Hampshire Total.Health.Spending        11954 2012
    ## 1124           New Jersey Total.Health.Spending        73373 2012
    ## 1125           New Mexico Total.Health.Spending        14109 2012
    ## 1126             New York Total.Health.Spending       177908 2012
    ## 1127       North Carolina Total.Health.Spending        68932 2012
    ## 1128         North Dakota Total.Health.Spending         6303 2012
    ## 1129                 Ohio Total.Health.Spending        93242 2012
    ## 1130             Oklahoma Total.Health.Spending        27386 2012
    ## 1131               Oregon Total.Health.Spending        27757 2012
    ## 1132         Pennsylvania Total.Health.Spending       110252 2012
    ## 1133         Rhode Island Total.Health.Spending         9435 2012
    ## 1134       South Carolina Total.Health.Spending        32351 2012
    ## 1135         South Dakota Total.Health.Spending         6955 2012
    ## 1136            Tennessee Total.Health.Spending        45222 2012
    ## 1137                Texas Total.Health.Spending       170992 2012
    ## 1138                 Utah Total.Health.Spending        15830 2012
    ## 1139              Vermont Total.Health.Spending         5827 2012
    ## 1140             Virginia Total.Health.Spending        58535 2012
    ## 1141           Washington Total.Health.Spending        51443 2012
    ## 1142        West Virginia Total.Health.Spending        16270 2012
    ## 1143            Wisconsin Total.Health.Spending        46158 2012
    ## 1144              Wyoming Total.Health.Spending         4518 2012
    ## 1145        United States Total.Health.Spending      2435624 2013
    ## 1146              Alabama Total.Health.Spending        33788 2013
    ## 1147               Alaska Total.Health.Spending         7684 2013
    ## 1148              Arizona Total.Health.Spending        41481 2013
    ## 1149             Arkansas Total.Health.Spending        20500 2013
    ## 1150           California Total.Health.Spending       278168 2013
    ## 1151             Colorado Total.Health.Spending        34090 2013
    ## 1152          Connecticut Total.Health.Spending        34223 2013
    ## 1153             Delaware Total.Health.Spending         9038 2013
    ## 1154 District of Columbia Total.Health.Spending         7443 2013
    ## 1155              Florida Total.Health.Spending       150547 2013
    ## 1156              Georgia Total.Health.Spending        62399 2013
    ## 1157               Hawaii Total.Health.Spending         9781 2013
    ## 1158                Idaho Total.Health.Spending        10627 2013
    ## 1159             Illinois Total.Health.Spending       101891 2013
    ## 1160              Indiana Total.Health.Spending        52046 2013
    ## 1161                 Iowa Total.Health.Spending        24135 2013
    ## 1162               Kansas Total.Health.Spending        21490 2013
    ## 1163             Kentucky Total.Health.Spending        33194 2013
    ## 1164            Louisiana Total.Health.Spending        34639 2013
    ## 1165                Maine Total.Health.Spending        12139 2013
    ## 1166             Maryland Total.Health.Spending        48929 2013
    ## 1167        Massachusetts Total.Health.Spending        68899 2013
    ## 1168             Michigan Total.Health.Spending        76672 2013
    ## 1169            Minnesota Total.Health.Spending        45865 2013
    ## 1170          Mississippi Total.Health.Spending        22017 2013
    ## 1171             Missouri Total.Health.Spending        47499 2013
    ## 1172              Montana Total.Health.Spending         8108 2013
    ## 1173             Nebraska Total.Health.Spending        15197 2013
    ## 1174               Nevada Total.Health.Spending        17485 2013
    ## 1175        New Hampshire Total.Health.Spending        12392 2013
    ## 1176           New Jersey Total.Health.Spending        75148 2013
    ## 1177           New Mexico Total.Health.Spending        14304 2013
    ## 1178             New York Total.Health.Spending       183969 2013
    ## 1179       North Carolina Total.Health.Spending        69157 2013
    ## 1180         North Dakota Total.Health.Spending         6795 2013
    ## 1181                 Ohio Total.Health.Spending        95866 2013
    ## 1182             Oklahoma Total.Health.Spending        28097 2013
    ## 1183               Oregon Total.Health.Spending        29314 2013
    ## 1184         Pennsylvania Total.Health.Spending       113459 2013
    ## 1185         Rhode Island Total.Health.Spending         9646 2013
    ## 1186       South Carolina Total.Health.Spending        33468 2013
    ## 1187         South Dakota Total.Health.Spending         7221 2013
    ## 1188            Tennessee Total.Health.Spending        46149 2013
    ## 1189                Texas Total.Health.Spending       176341 2013
    ## 1190                 Utah Total.Health.Spending        16425 2013
    ## 1191              Vermont Total.Health.Spending         6221 2013
    ## 1192             Virginia Total.Health.Spending        60364 2013
    ## 1193           Washington Total.Health.Spending        53022 2013
    ## 1194        West Virginia Total.Health.Spending        16622 2013
    ## 1195            Wisconsin Total.Health.Spending        47030 2013
    ## 1196              Wyoming Total.Health.Spending         4639 2013
    ## 1197        United States Total.Health.Spending      2562824 2014
    ## 1198              Alabama Total.Health.Spending        35263 2014
    ## 1199               Alaska Total.Health.Spending         8151 2014
    ## 1200              Arizona Total.Health.Spending        43356 2014
    ## 1201             Arkansas Total.Health.Spending        21980 2014
    ## 1202           California Total.Health.Spending       291989 2014
    ## 1203             Colorado Total.Health.Spending        36398 2014
    ## 1204          Connecticut Total.Health.Spending        35413 2014
    ## 1205             Delaware Total.Health.Spending         9587 2014
    ## 1206 District of Columbia Total.Health.Spending         7871 2014
    ## 1207              Florida Total.Health.Spending       160624 2014
    ## 1208              Georgia Total.Health.Spending        66447 2014
    ## 1209               Hawaii Total.Health.Spending        10338 2014
    ## 1210                Idaho Total.Health.Spending        11315 2014
    ## 1211             Illinois Total.Health.Spending       106306 2014
    ## 1212              Indiana Total.Health.Spending        54741 2014
    ## 1213                 Iowa Total.Health.Spending        25487 2014
    ## 1214               Kansas Total.Health.Spending        22183 2014
    ## 1215             Kentucky Total.Health.Spending        35323 2014
    ## 1216            Louisiana Total.Health.Spending        36324 2014
    ## 1217                Maine Total.Health.Spending        12684 2014
    ## 1218             Maryland Total.Health.Spending        51330 2014
    ## 1219        Massachusetts Total.Health.Spending        71274 2014
    ## 1220             Michigan Total.Health.Spending        79874 2014
    ## 1221            Minnesota Total.Health.Spending        48377 2014
    ## 1222          Mississippi Total.Health.Spending        22879 2014
    ## 1223             Missouri Total.Health.Spending        49137 2014
    ## 1224              Montana Total.Health.Spending         8409 2014
    ## 1225             Nebraska Total.Health.Spending        15823 2014
    ## 1226               Nevada Total.Health.Spending        19020 2014
    ## 1227        New Hampshire Total.Health.Spending        12742 2014
    ## 1228           New Jersey Total.Health.Spending        79066 2014
    ## 1229           New Mexico Total.Health.Spending        15027 2014
    ## 1230             New York Total.Health.Spending       192809 2014
    ## 1231       North Carolina Total.Health.Spending        72160 2014
    ## 1232         North Dakota Total.Health.Spending         7289 2014
    ## 1233                 Ohio Total.Health.Spending       101013 2014
    ## 1234             Oklahoma Total.Health.Spending        29575 2014
    ## 1235               Oregon Total.Health.Spending        31920 2014
    ## 1236         Pennsylvania Total.Health.Spending       118419 2014
    ## 1237         Rhode Island Total.Health.Spending        10071 2014
    ## 1238       South Carolina Total.Health.Spending        35299 2014
    ## 1239         South Dakota Total.Health.Spending         7616 2014
    ## 1240            Tennessee Total.Health.Spending        48249 2014
    ## 1241                Texas Total.Health.Spending       188559 2014
    ## 1242                 Utah Total.Health.Spending        17597 2014
    ## 1243              Vermont Total.Health.Spending         6389 2014
    ## 1244             Virginia Total.Health.Spending        62847 2014
    ## 1245           Washington Total.Health.Spending        55819 2014
    ## 1246        West Virginia Total.Health.Spending        17491 2014
    ## 1247            Wisconsin Total.Health.Spending        50109 2014
    ## 1248              Wyoming Total.Health.Spending         4856 2014

4.2 Merge two data frames: the resulting data frame should contain information about coverage and expenditures for years 2013-2016. Please note that file **expenditures.csv** does not contain years 2015-2016.

``` r
merge(tidy.coverage, tidy.expenditures, by = c("Location", "Year"), all = T) %>%
  filter(Year >= 2013 & Year <= 2016)
```

    ##                  Location Year   Category.x     Cases
    ## 1                 Alabama 2013    Uninsured    724800
    ## 2                 Alabama 2013     Medicare    783000
    ## 3                 Alabama 2013     Employer   2126500
    ## 4                 Alabama 2013        Total   4763900
    ## 5                 Alabama 2013 Other.Public     85600
    ## 6                 Alabama 2013    Non.Group    174200
    ## 7                 Alabama 2013     Medicaid    869700
    ## 8                 Alabama 2014    Uninsured    522200
    ## 9                 Alabama 2014     Medicare    718400
    ## 10                Alabama 2014     Employer   2202800
    ## 11                Alabama 2014        Total   4768000
    ## 12                Alabama 2014 Other.Public    143900
    ## 13                Alabama 2014    Non.Group    288900
    ## 14                Alabama 2014     Medicaid    891900
    ## 15                Alabama 2015     Employer   2218000
    ## 16                Alabama 2015    Non.Group    291500
    ## 17                Alabama 2015     Medicaid    911400
    ## 18                Alabama 2015     Medicare    719100
    ## 19                Alabama 2015 Other.Public    174600
    ## 20                Alabama 2015    Uninsured    519400
    ## 21                Alabama 2015        Total   4833900
    ## 22                Alabama 2016     Employer   2263800
    ## 23                Alabama 2016    Non.Group    262400
    ## 24                Alabama 2016     Medicaid    997000
    ## 25                Alabama 2016     Medicare    761200
    ## 26                Alabama 2016 Other.Public    128800
    ## 27                Alabama 2016    Uninsured    420800
    ## 28                Alabama 2016        Total   4834100
    ## 29                 Alaska 2013        Total    702000
    ## 30                 Alaska 2013     Employer    364900
    ## 31                 Alaska 2013    Uninsured    102200
    ## 32                 Alaska 2013    Non.Group     24000
    ## 33                 Alaska 2013     Medicare     55200
    ## 34                 Alaska 2013 Other.Public     60600
    ## 35                 Alaska 2013     Medicaid     95000
    ## 36                 Alaska 2014        Total    695700
    ## 37                 Alaska 2014     Employer    345300
    ## 38                 Alaska 2014    Uninsured    100800
    ## 39                 Alaska 2014    Non.Group     26800
    ## 40                 Alaska 2014     Medicare     55300
    ## 41                 Alaska 2014 Other.Public     37300
    ## 42                 Alaska 2014     Medicaid    130100
    ## 43                 Alaska 2015     Employer    355700
    ## 44                 Alaska 2015    Non.Group     22300
    ## 45                 Alaska 2015     Medicaid    128100
    ## 46                 Alaska 2015     Medicare     60900
    ## 47                 Alaska 2015 Other.Public     47700
    ## 48                 Alaska 2015    Uninsured     90500
    ## 49                 Alaska 2015        Total    705300
    ## 50                 Alaska 2016     Employer    324400
    ## 51                 Alaska 2016    Non.Group     20300
    ## 52                 Alaska 2016     Medicaid    145400
    ## 53                 Alaska 2016     Medicare     68200
    ## 54                 Alaska 2016 Other.Public     55600
    ## 55                 Alaska 2016    Uninsured     96900
    ## 56                 Alaska 2016        Total    710800
    ## 57                Arizona 2013     Employer   2883800
    ## 58                Arizona 2013    Non.Group    170800
    ## 59                Arizona 2013    Uninsured   1223000
    ## 60                Arizona 2013 Other.Public       N/A
    ## 61                Arizona 2013        Total   6603100
    ## 62                Arizona 2013     Medicaid   1346100
    ## 63                Arizona 2013     Medicare    842000
    ## 64                Arizona 2014     Employer   2835200
    ## 65                Arizona 2014    Non.Group    333500
    ## 66                Arizona 2014    Uninsured    827100
    ## 67                Arizona 2014 Other.Public       N/A
    ## 68                Arizona 2014        Total   6657200
    ## 69                Arizona 2014     Medicaid   1639400
    ## 70                Arizona 2014     Medicare    911100
    ## 71                Arizona 2015     Employer   2766500
    ## 72                Arizona 2015    Non.Group    278400
    ## 73                Arizona 2015     Medicaid   1711500
    ## 74                Arizona 2015     Medicare    949000
    ## 75                Arizona 2015 Other.Public    189300
    ## 76                Arizona 2015    Uninsured    844800
    ## 77                Arizona 2015        Total   6739500
    ## 78                Arizona 2016     Employer   3010700
    ## 79                Arizona 2016    Non.Group    377000
    ## 80                Arizona 2016     Medicaid   1468400
    ## 81                Arizona 2016     Medicare   1028000
    ## 82                Arizona 2016 Other.Public    172500
    ## 83                Arizona 2016    Uninsured    833700
    ## 84                Arizona 2016        Total   6890200
    ## 85               Arkansas 2013    Non.Group    155600
    ## 86               Arkansas 2013     Medicaid    600800
    ## 87               Arkansas 2013     Employer   1128800
    ## 88               Arkansas 2013    Uninsured    436800
    ## 89               Arkansas 2013 Other.Public     67600
    ## 90               Arkansas 2013        Total   2904800
    ## 91               Arkansas 2013     Medicare    515200
    ## 92               Arkansas 2014    Non.Group    231700
    ## 93               Arkansas 2014     Medicaid    639200
    ## 94               Arkansas 2014     Employer   1176500
    ## 95               Arkansas 2014    Uninsured    287200
    ## 96               Arkansas 2014 Other.Public     82000
    ## 97               Arkansas 2014        Total   2896000
    ## 98               Arkansas 2014     Medicare    479400
    ## 99               Arkansas 2015     Employer   1293700
    ## 100              Arkansas 2015    Non.Group    200200
    ## 101              Arkansas 2015     Medicaid    641400
    ## 102              Arkansas 2015     Medicare    484500
    ## 103              Arkansas 2015 Other.Public     63700
    ## 104              Arkansas 2015    Uninsured    268400
    ## 105              Arkansas 2015        Total   2953000
    ## 106              Arkansas 2016     Employer   1290900
    ## 107              Arkansas 2016    Non.Group    252900
    ## 108              Arkansas 2016     Medicaid    618600
    ## 109              Arkansas 2016     Medicare    490000
    ## 110              Arkansas 2016 Other.Public     67500
    ## 111              Arkansas 2016    Uninsured    225500
    ## 112              Arkansas 2016        Total   2945300
    ## 113            California 2013    Uninsured   5594100
    ## 114            California 2013     Medicare   3828500
    ## 115            California 2013     Medicaid   8344800
    ## 116            California 2013    Non.Group   1986400
    ## 117            California 2013     Employer  17747300
    ## 118            California 2013 Other.Public    675400
    ## 119            California 2013        Total  38176400
    ## 120            California 2014    Uninsured   3916700
    ## 121            California 2014     Medicare   4049000
    ## 122            California 2014     Medicaid   9618800
    ## 123            California 2014    Non.Group   2778800
    ## 124            California 2014     Employer  17703700
    ## 125            California 2014 Other.Public    634400
    ## 126            California 2014        Total  38701300
    ## 127            California 2015     Employer  17718300
    ## 128            California 2015    Non.Group   3444200
    ## 129            California 2015     Medicaid  10138100
    ## 130            California 2015     Medicare   4080100
    ## 131            California 2015 Other.Public    752700
    ## 132            California 2015    Uninsured   2980600
    ## 133            California 2015        Total  39113900
    ## 134            California 2016     Employer  18116200
    ## 135            California 2016    Non.Group   3195400
    ## 136            California 2016     Medicaid   9853800
    ## 137            California 2016     Medicare   4436000
    ## 138            California 2016 Other.Public    556100
    ## 139            California 2016    Uninsured   3030800
    ## 140            California 2016        Total  39188300
    ## 141              Colorado 2013        Total   5297800
    ## 142              Colorado 2013    Uninsured    654000
    ## 143              Colorado 2013     Employer   2852500
    ## 144              Colorado 2013     Medicare    549700
    ## 145              Colorado 2013     Medicaid    697300
    ## 146              Colorado 2013    Non.Group    426300
    ## 147              Colorado 2013 Other.Public    118100
    ## 148              Colorado 2014        Total   5377400
    ## 149              Colorado 2014    Uninsured    602900
    ## 150              Colorado 2014     Employer   2489400
    ## 151              Colorado 2014     Medicare    619500
    ## 152              Colorado 2014     Medicaid   1053700
    ## 153              Colorado 2014    Non.Group    397900
    ## 154              Colorado 2014 Other.Public    214000
    ## 155              Colorado 2015     Employer   2706000
    ## 156              Colorado 2015    Non.Group    346900
    ## 157              Colorado 2015     Medicaid   1036600
    ## 158              Colorado 2015     Medicare    708000
    ## 159              Colorado 2015 Other.Public    148000
    ## 160              Colorado 2015    Uninsured    475700
    ## 161              Colorado 2015        Total   5421300
    ## 162              Colorado 2016     Employer   2872600
    ## 163              Colorado 2016    Non.Group    370000
    ## 164              Colorado 2016     Medicaid    855800
    ## 165              Colorado 2016     Medicare    692400
    ## 166              Colorado 2016 Other.Public    190100
    ## 167              Colorado 2016    Uninsured    528400
    ## 168              Colorado 2016        Total   5509200
    ## 169           Connecticut 2013     Medicare    475300
    ## 170           Connecticut 2013     Employer   2030500
    ## 171           Connecticut 2013    Non.Group    126800
    ## 172           Connecticut 2013        Total   3578900
    ## 173           Connecticut 2013 Other.Public     48200
    ## 174           Connecticut 2013    Uninsured    366100
    ## 175           Connecticut 2013     Medicaid    532000
    ## 176           Connecticut 2014     Medicare    435400
    ## 177           Connecticut 2014     Employer   2086800
    ## 178           Connecticut 2014    Non.Group    223500
    ## 179           Connecticut 2014        Total   3577900
    ## 180           Connecticut 2014 Other.Public     40900
    ## 181           Connecticut 2014    Uninsured    249200
    ## 182           Connecticut 2014     Medicaid    542000
    ## 183           Connecticut 2015     Employer   1857800
    ## 184           Connecticut 2015    Non.Group    302700
    ## 185           Connecticut 2015     Medicaid    690400
    ## 186           Connecticut 2015     Medicare    474700
    ## 187           Connecticut 2015 Other.Public       N/A
    ## 188           Connecticut 2015    Uninsured    216900
    ## 189           Connecticut 2015        Total   3571700
    ## 190           Connecticut 2016     Employer   1926900
    ## 191           Connecticut 2016    Non.Group    200700
    ## 192           Connecticut 2016     Medicaid    711200
    ## 193           Connecticut 2016     Medicare    483300
    ## 194           Connecticut 2016 Other.Public       N/A
    ## 195           Connecticut 2016    Uninsured    218600
    ## 196           Connecticut 2016        Total   3570300
    ## 197              Delaware 2013     Medicare    141300
    ## 198              Delaware 2013 Other.Public     13800
    ## 199              Delaware 2013     Employer    473700
    ## 200              Delaware 2013     Medicaid    192700
    ## 201              Delaware 2013    Non.Group     25100
    ## 202              Delaware 2013    Uninsured     62700
    ## 203              Delaware 2013        Total    909300
    ## 204              Delaware 2014     Medicare    141100
    ## 205              Delaware 2014 Other.Public     30700
    ## 206              Delaware 2014     Employer    488100
    ## 207              Delaware 2014     Medicaid    167200
    ## 208              Delaware 2014    Non.Group     39200
    ## 209              Delaware 2014    Uninsured     63000
    ## 210              Delaware 2014        Total    929500
    ## 211              Delaware 2015     Employer    505800
    ## 212              Delaware 2015    Non.Group     48600
    ## 213              Delaware 2015     Medicaid    174700
    ## 214              Delaware 2015     Medicare    138200
    ## 215              Delaware 2015 Other.Public     26500
    ## 216              Delaware 2015    Uninsured     65300
    ## 217              Delaware 2015        Total    959100
    ## 218              Delaware 2016     Employer    445500
    ## 219              Delaware 2016    Non.Group     36200
    ## 220              Delaware 2016     Medicaid    211900
    ## 221              Delaware 2016     Medicare    152000
    ## 222              Delaware 2016 Other.Public     18600
    ## 223              Delaware 2016    Uninsured     84100
    ## 224              Delaware 2016        Total    948200
    ## 225  District of Columbia 2013     Medicare     59900
    ## 226  District of Columbia 2013    Uninsured     51100
    ## 227  District of Columbia 2013 Other.Public       N/A
    ## 228  District of Columbia 2013    Non.Group     30400
    ## 229  District of Columbia 2013        Total    652100
    ## 230  District of Columbia 2013     Medicaid    174900
    ## 231  District of Columbia 2013     Employer    324300
    ## 232  District of Columbia 2014     Medicare     51900
    ## 233  District of Columbia 2014    Uninsured     42300
    ## 234  District of Columbia 2014 Other.Public       N/A
    ## 235  District of Columbia 2014    Non.Group     39900
    ## 236  District of Columbia 2014        Total    656900
    ## 237  District of Columbia 2014     Medicaid    162600
    ## 238  District of Columbia 2014     Employer    354100
    ## 239  District of Columbia 2015     Employer    349900
    ## 240  District of Columbia 2015    Non.Group     53500
    ## 241  District of Columbia 2015     Medicaid    175400
    ## 242  District of Columbia 2015     Medicare     66400
    ## 243  District of Columbia 2015 Other.Public       N/A
    ## 244  District of Columbia 2015    Uninsured     27200
    ## 245  District of Columbia 2015        Total    676800
    ## 246  District of Columbia 2016     Employer    351800
    ## 247  District of Columbia 2016    Non.Group     48100
    ## 248  District of Columbia 2016     Medicaid    166000
    ## 249  District of Columbia 2016     Medicare     76000
    ## 250  District of Columbia 2016 Other.Public       N/A
    ## 251  District of Columbia 2016    Uninsured     35400
    ## 252  District of Columbia 2016        Total    685800
    ## 253               Florida 2013    Uninsured   3619700
    ## 254               Florida 2013     Employer   8023400
    ## 255               Florida 2013        Total  19429000
    ## 256               Florida 2013 Other.Public    517800
    ## 257               Florida 2013     Medicare   3108800
    ## 258               Florida 2013     Medicaid   3190900
    ## 259               Florida 2013    Non.Group    968200
    ## 260               Florida 2014    Uninsured   2874800
    ## 261               Florida 2014     Employer   7731600
    ## 262               Florida 2014        Total  19731100
    ## 263               Florida 2014 Other.Public    473300
    ## 264               Florida 2014     Medicare   3324700
    ## 265               Florida 2014     Medicaid   3755500
    ## 266               Florida 2014    Non.Group   1563400
    ## 267               Florida 2015     Employer   7879800
    ## 268               Florida 2015    Non.Group   1949500
    ## 269               Florida 2015     Medicaid   3610400
    ## 270               Florida 2015     Medicare   3535600
    ## 271               Florida 2015 Other.Public    563200
    ## 272               Florida 2015    Uninsured   2544700
    ## 273               Florida 2015        Total  20085300
    ## 274               Florida 2016     Employer   8602700
    ## 275               Florida 2016    Non.Group   2061400
    ## 276               Florida 2016     Medicaid   3294400
    ## 277               Florida 2016     Medicare   3602900
    ## 278               Florida 2016 Other.Public    518200
    ## 279               Florida 2016    Uninsured   2465800
    ## 280               Florida 2016        Total  20545300
    ## 281               Georgia 2013    Uninsured   1592800
    ## 282               Georgia 2013     Medicare   1280400
    ## 283               Georgia 2013     Employer   4700500
    ## 284               Georgia 2013    Non.Group    401600
    ## 285               Georgia 2013        Total   9813000
    ## 286               Georgia 2013 Other.Public    334700
    ## 287               Georgia 2013     Medicaid   1503000
    ## 288               Georgia 2014    Uninsured   1546500
    ## 289               Georgia 2014     Medicare   1230400
    ## 290               Georgia 2014     Employer   4697300
    ## 291               Georgia 2014    Non.Group    551200
    ## 292               Georgia 2014        Total   9965100
    ## 293               Georgia 2014 Other.Public    296700
    ## 294               Georgia 2014     Medicaid   1643000
    ## 295               Georgia 2015     Employer   4649000
    ## 296               Georgia 2015    Non.Group    583200
    ## 297               Georgia 2015     Medicaid   1914400
    ## 298               Georgia 2015     Medicare   1271300
    ## 299               Georgia 2015 Other.Public    275800
    ## 300               Georgia 2015    Uninsured   1411000
    ## 301               Georgia 2015        Total  10104900
    ## 302               Georgia 2016     Employer   5038000
    ## 303               Georgia 2016    Non.Group    695300
    ## 304               Georgia 2016     Medicaid   1708000
    ## 305               Georgia 2016     Medicare   1213800
    ## 306               Georgia 2016 Other.Public    360000
    ## 307               Georgia 2016    Uninsured   1265600
    ## 308               Georgia 2016        Total  10280800
    ## 309                Hawaii 2013        Total   1355600
    ## 310                Hawaii 2013     Medicare    195000
    ## 311                Hawaii 2013    Uninsured     68600
    ## 312                Hawaii 2013 Other.Public     78600
    ## 313                Hawaii 2013    Non.Group     47900
    ## 314                Hawaii 2013     Employer    732600
    ## 315                Hawaii 2013     Medicaid    232600
    ## 316                Hawaii 2014        Total   1365400
    ## 317                Hawaii 2014     Medicare    205500
    ## 318                Hawaii 2014    Uninsured     72800
    ## 319                Hawaii 2014 Other.Public     89900
    ## 320                Hawaii 2014    Non.Group     44000
    ## 321                Hawaii 2014     Employer    724800
    ## 322                Hawaii 2014     Medicaid    228300
    ## 323                Hawaii 2015     Employer    720000
    ## 324                Hawaii 2015    Non.Group     38300
    ## 325                Hawaii 2015     Medicaid    243800
    ## 326                Hawaii 2015     Medicare    207100
    ## 327                Hawaii 2015 Other.Public    106500
    ## 328                Hawaii 2015    Uninsured     70300
    ## 329                Hawaii 2015        Total   1386000
    ## 330                Hawaii 2016     Employer    771400
    ## 331                Hawaii 2016    Non.Group     47500
    ## 332                Hawaii 2016     Medicaid    207900
    ## 333                Hawaii 2016     Medicare    210200
    ## 334                Hawaii 2016 Other.Public     80400
    ## 335                Hawaii 2016    Uninsured     75000
    ## 336                Hawaii 2016        Total   1392400
    ## 337                 Idaho 2013        Total   1600600
    ## 338                 Idaho 2013     Medicare    194600
    ## 339                 Idaho 2013     Employer    802200
    ## 340                 Idaho 2013    Uninsured    235000
    ## 341                 Idaho 2013 Other.Public     21500
    ## 342                 Idaho 2013    Non.Group    114100
    ## 343                 Idaho 2013     Medicaid    233200
    ## 344                 Idaho 2014        Total   1610200
    ## 345                 Idaho 2014     Medicare    181900
    ## 346                 Idaho 2014     Employer    825700
    ## 347                 Idaho 2014    Uninsured    169800
    ## 348                 Idaho 2014 Other.Public     37300
    ## 349                 Idaho 2014    Non.Group    126900
    ## 350                 Idaho 2014     Medicaid    268600
    ## 351                 Idaho 2015     Employer    798000
    ## 352                 Idaho 2015    Non.Group    124600
    ## 353                 Idaho 2015     Medicaid    292700
    ## 354                 Idaho 2015     Medicare    232500
    ## 355                 Idaho 2015 Other.Public     24200
    ## 356                 Idaho 2015    Uninsured    187500
    ## 357                 Idaho 2015        Total   1659500
    ## 358                 Idaho 2016     Employer    811900
    ## 359                 Idaho 2016    Non.Group    164600
    ## 360                 Idaho 2016     Medicaid    293500
    ## 361                 Idaho 2016     Medicare    229500
    ## 362                 Idaho 2016 Other.Public     32500
    ## 363                 Idaho 2016    Uninsured    147100
    ## 364                 Idaho 2016        Total   1679000
    ## 365              Illinois 2013    Uninsured   1335800
    ## 366              Illinois 2013     Employer   6768200
    ## 367              Illinois 2013        Total  12769500
    ## 368              Illinois 2013     Medicare   1539500
    ## 369              Illinois 2013 Other.Public    110200
    ## 370              Illinois 2013    Non.Group    636900
    ## 371              Illinois 2013     Medicaid   2379100
    ## 372              Illinois 2014    Uninsured   1141300
    ## 373              Illinois 2014     Employer   6629700
    ## 374              Illinois 2014        Total  12797900
    ## 375              Illinois 2014     Medicare   1754400
    ## 376              Illinois 2014 Other.Public    114400
    ## 377              Illinois 2014    Non.Group    722000
    ## 378              Illinois 2014     Medicaid   2436100
    ## 379              Illinois 2015     Employer   6842600
    ## 380              Illinois 2015    Non.Group    812600
    ## 381              Illinois 2015     Medicaid   2405700
    ## 382              Illinois 2015     Medicare   1741700
    ## 383              Illinois 2015 Other.Public    106900
    ## 384              Illinois 2015    Uninsured    792300
    ## 385              Illinois 2015        Total  12701800
    ## 386              Illinois 2016     Employer   6491400
    ## 387              Illinois 2016    Non.Group    965000
    ## 388              Illinois 2016     Medicaid   2321100
    ## 389              Illinois 2016     Medicare   1761300
    ## 390              Illinois 2016 Other.Public    137600
    ## 391              Illinois 2016    Uninsured    931100
    ## 392              Illinois 2016        Total  12607400
    ## 393               Indiana 2013     Employer   3494600
    ## 394               Indiana 2013     Medicare    830300
    ## 395               Indiana 2013    Non.Group    234300
    ## 396               Indiana 2013    Uninsured    814200
    ## 397               Indiana 2013        Total   6471300
    ## 398               Indiana 2013 Other.Public     91700
    ## 399               Indiana 2013     Medicaid   1006200
    ## 400               Indiana 2014     Employer   3296700
    ## 401               Indiana 2014     Medicare    980700
    ## 402               Indiana 2014    Non.Group    341100
    ## 403               Indiana 2014    Uninsured    687900
    ## 404               Indiana 2014        Total   6477500
    ## 405               Indiana 2014 Other.Public       N/A
    ## 406               Indiana 2014     Medicaid   1095800
    ## 407               Indiana 2015     Employer   3383600
    ## 408               Indiana 2015    Non.Group    317100
    ## 409               Indiana 2015     Medicaid   1218500
    ## 410               Indiana 2015     Medicare    906800
    ## 411               Indiana 2015 Other.Public     74900
    ## 412               Indiana 2015    Uninsured    611000
    ## 413               Indiana 2015        Total   6512100
    ## 414               Indiana 2016     Employer   3379100
    ## 415               Indiana 2016    Non.Group     4e+05
    ## 416               Indiana 2016     Medicaid   1279300
    ## 417               Indiana 2016     Medicare    922500
    ## 418               Indiana 2016 Other.Public     94300
    ## 419               Indiana 2016    Uninsured    428100
    ## 420               Indiana 2016        Total   6503200
    ## 421                  Iowa 2013     Employer   1743100
    ## 422                  Iowa 2013    Non.Group    168800
    ## 423                  Iowa 2013        Total   3077400
    ## 424                  Iowa 2013     Medicare    404100
    ## 425                  Iowa 2013    Uninsured    250800
    ## 426                  Iowa 2013 Other.Public     46600
    ## 427                  Iowa 2013     Medicaid    464000
    ## 428                  Iowa 2014     Employer   1643600
    ## 429                  Iowa 2014    Non.Group    237400
    ## 430                  Iowa 2014        Total   3080800
    ## 431                  Iowa 2014     Medicare    443500
    ## 432                  Iowa 2014    Uninsured    191800
    ## 433                  Iowa 2014 Other.Public     51600
    ## 434                  Iowa 2014     Medicaid    512900
    ## 435                  Iowa 2015     Employer   1641000
    ## 436                  Iowa 2015    Non.Group    249700
    ## 437                  Iowa 2015     Medicaid    538700
    ## 438                  Iowa 2015     Medicare    459600
    ## 439                  Iowa 2015 Other.Public     45000
    ## 440                  Iowa 2015    Uninsured    166700
    ## 441                  Iowa 2015        Total   3100600
    ## 442                  Iowa 2016     Employer   1690700
    ## 443                  Iowa 2016    Non.Group    191800
    ## 444                  Iowa 2016     Medicaid    571300
    ## 445                  Iowa 2016     Medicare    459800
    ## 446                  Iowa 2016 Other.Public     30200
    ## 447                  Iowa 2016    Uninsured    162300
    ## 448                  Iowa 2016        Total   3106200
    ## 449                Kansas 2013    Non.Group    127900
    ## 450                Kansas 2013     Employer   1530500
    ## 451                Kansas 2013     Medicaid    414600
    ## 452                Kansas 2013    Uninsured    283000
    ## 453                Kansas 2013        Total   2817600
    ## 454                Kansas 2013     Medicare    364900
    ## 455                Kansas 2013 Other.Public       N/A
    ## 456                Kansas 2014    Non.Group    167000
    ## 457                Kansas 2014     Employer   1495700
    ## 458                Kansas 2014     Medicaid    430100
    ## 459                Kansas 2014    Uninsured    307400
    ## 460                Kansas 2014        Total   2853000
    ## 461                Kansas 2014     Medicare    373100
    ## 462                Kansas 2014 Other.Public       N/A
    ## 463                Kansas 2015     Employer   1542700
    ## 464                Kansas 2015    Non.Group    206900
    ## 465                Kansas 2015     Medicaid    384300
    ## 466                Kansas 2015     Medicare    370600
    ## 467                Kansas 2015 Other.Public     62600
    ## 468                Kansas 2015    Uninsured    285300
    ## 469                Kansas 2015        Total   2852400
    ## 470                Kansas 2016     Employer   1506300
    ## 471                Kansas 2016    Non.Group    246300
    ## 472                Kansas 2016     Medicaid    412200
    ## 473                Kansas 2016     Medicare    380200
    ## 474                Kansas 2016 Other.Public       N/A
    ## 475                Kansas 2016    Uninsured    241900
    ## 476                Kansas 2016        Total   2865000
    ## 477              Kentucky 2013    Uninsured    624100
    ## 478              Kentucky 2013     Medicaid    868400
    ## 479              Kentucky 2013    Non.Group    110100
    ## 480              Kentucky 2013     Employer   2076800
    ## 481              Kentucky 2013        Total   4400100
    ## 482              Kentucky 2013     Medicare    604700
    ## 483              Kentucky 2013 Other.Public    116000
    ## 484              Kentucky 2014    Uninsured    289300
    ## 485              Kentucky 2014     Medicaid    987300
    ## 486              Kentucky 2014    Non.Group    271900
    ## 487              Kentucky 2014     Employer   1965500
    ## 488              Kentucky 2014        Total   4315700
    ## 489              Kentucky 2014     Medicare    713800
    ## 490              Kentucky 2014 Other.Public       N/A
    ## 491              Kentucky 2015     Employer   1983800
    ## 492              Kentucky 2015    Non.Group    378100
    ## 493              Kentucky 2015     Medicaid    964400
    ## 494              Kentucky 2015     Medicare    719500
    ## 495              Kentucky 2015 Other.Public       N/A
    ## 496              Kentucky 2015    Uninsured    266900
    ## 497              Kentucky 2015        Total   4383400
    ## 498              Kentucky 2016     Employer   1948800
    ## 499              Kentucky 2016    Non.Group    428000
    ## 500              Kentucky 2016     Medicaid    928000
    ## 501              Kentucky 2016     Medicare    706300
    ## 502              Kentucky 2016 Other.Public       N/A
    ## 503              Kentucky 2016    Uninsured    271400
    ## 504              Kentucky 2016        Total   4388200
    ## 505             Louisiana 2013     Medicaid    900200
    ## 506             Louisiana 2013     Employer   2083400
    ## 507             Louisiana 2013     Medicare    643900
    ## 508             Louisiana 2013    Uninsured    645000
    ## 509             Louisiana 2013    Non.Group    164700
    ## 510             Louisiana 2013        Total   4532300
    ## 511             Louisiana 2013 Other.Public     95100
    ## 512             Louisiana 2014     Medicaid   1036100
    ## 513             Louisiana 2014     Employer   2067300
    ## 514             Louisiana 2014     Medicare    517100
    ## 515             Louisiana 2014    Uninsured    588000
    ## 516             Louisiana 2014    Non.Group    231100
    ## 517             Louisiana 2014        Total   4556500
    ## 518             Louisiana 2014 Other.Public    117000
    ## 519             Louisiana 2015     Employer   2125700
    ## 520             Louisiana 2015    Non.Group    315100
    ## 521             Louisiana 2015     Medicaid    921700
    ## 522             Louisiana 2015     Medicare    615600
    ## 523             Louisiana 2015 Other.Public       N/A
    ## 524             Louisiana 2015    Uninsured    502900
    ## 525             Louisiana 2015        Total   4604200
    ## 526             Louisiana 2016     Employer   1910100
    ## 527             Louisiana 2016    Non.Group    281500
    ## 528             Louisiana 2016     Medicaid   1136600
    ## 529             Louisiana 2016     Medicare    621100
    ## 530             Louisiana 2016 Other.Public       N/A
    ## 531             Louisiana 2016    Uninsured    484500
    ## 532             Louisiana 2016        Total   4578500
    ## 533                 Maine 2013     Medicaid    277200
    ## 534                 Maine 2013     Medicare    195100
    ## 535                 Maine 2013    Non.Group     41300
    ## 536                 Maine 2013     Employer    658200
    ## 537                 Maine 2013        Total   1316500
    ## 538                 Maine 2013 Other.Public     21200
    ## 539                 Maine 2013    Uninsured    123600
    ## 540                 Maine 2014     Medicaid    255400
    ## 541                 Maine 2014     Medicare    213500
    ## 542                 Maine 2014    Non.Group     67000
    ## 543                 Maine 2014     Employer    616000
    ## 544                 Maine 2014        Total   1299600
    ## 545                 Maine 2014 Other.Public     26000
    ## 546                 Maine 2014    Uninsured    121800
    ## 547                 Maine 2015     Employer    634200
    ## 548                 Maine 2015    Non.Group     70100
    ## 549                 Maine 2015     Medicaid    306300
    ## 550                 Maine 2015     Medicare    245600
    ## 551                 Maine 2015 Other.Public     24900
    ## 552                 Maine 2015    Uninsured     60800
    ## 553                 Maine 2015        Total   1341900
    ## 554                 Maine 2016     Employer    645400
    ## 555                 Maine 2016    Non.Group     65600
    ## 556                 Maine 2016     Medicaid    280700
    ## 557                 Maine 2016     Medicare    217100
    ## 558                 Maine 2016 Other.Public     19000
    ## 559                 Maine 2016    Uninsured     93900
    ## 560                 Maine 2016        Total   1321600
    ## 561              Maryland 2013     Medicaid    889800
    ## 562              Maryland 2013     Medicare    751500
    ## 563              Maryland 2013     Employer   3172400
    ## 564              Maryland 2013 Other.Public    124400
    ## 565              Maryland 2013    Non.Group    320800
    ## 566              Maryland 2013    Uninsured    682000
    ## 567              Maryland 2013        Total   5940900
    ## 568              Maryland 2014     Medicaid    807900
    ## 569              Maryland 2014     Medicare    730900
    ## 570              Maryland 2014     Employer   3558800
    ## 571              Maryland 2014 Other.Public    136700
    ## 572              Maryland 2014    Non.Group    361700
    ## 573              Maryland 2014    Uninsured    343000
    ## 574              Maryland 2014        Total   5938900
    ## 575              Maryland 2015     Employer   3431400
    ## 576              Maryland 2015    Non.Group    371400
    ## 577              Maryland 2015     Medicaid    856800
    ## 578              Maryland 2015     Medicare    705500
    ## 579              Maryland 2015 Other.Public    141200
    ## 580              Maryland 2015    Uninsured    394300
    ## 581              Maryland 2015        Total   5900500
    ## 582              Maryland 2016     Employer   3210600
    ## 583              Maryland 2016    Non.Group    443000
    ## 584              Maryland 2016     Medicaid    926300
    ## 585              Maryland 2016     Medicare    827000
    ## 586              Maryland 2016 Other.Public    153800
    ## 587              Maryland 2016    Uninsured    372100
    ## 588              Maryland 2016        Total   5932800
    ## 589         Massachusetts 2013    Uninsured    208700
    ## 590         Massachusetts 2013     Medicare    840800
    ## 591         Massachusetts 2013     Medicaid   1356100
    ## 592         Massachusetts 2013        Total   6647700
    ## 593         Massachusetts 2013 Other.Public       N/A
    ## 594         Massachusetts 2013    Non.Group    289200
    ## 595         Massachusetts 2013     Employer   3908800
    ## 596         Massachusetts 2014    Uninsured    293800
    ## 597         Massachusetts 2014     Medicare    860500
    ## 598         Massachusetts 2014     Medicaid   1570100
    ## 599         Massachusetts 2014        Total   6658100
    ## 600         Massachusetts 2014 Other.Public       N/A
    ## 601         Massachusetts 2014    Non.Group    352200
    ## 602         Massachusetts 2014     Employer   3549900
    ## 603         Massachusetts 2015     Employer   3696000
    ## 604         Massachusetts 2015    Non.Group    368200
    ## 605         Massachusetts 2015     Medicaid   1575800
    ## 606         Massachusetts 2015     Medicare    792600
    ## 607         Massachusetts 2015 Other.Public     64300
    ## 608         Massachusetts 2015    Uninsured    288800
    ## 609         Massachusetts 2015        Total   6785700
    ## 610         Massachusetts 2016     Employer   3666500
    ## 611         Massachusetts 2016    Non.Group    392300
    ## 612         Massachusetts 2016     Medicaid   1473500
    ## 613         Massachusetts 2016     Medicare    797400
    ## 614         Massachusetts 2016 Other.Public     70800
    ## 615         Massachusetts 2016    Uninsured    379100
    ## 616         Massachusetts 2016        Total   6779600
    ## 617              Michigan 2013     Medicaid   1692500
    ## 618              Michigan 2013    Uninsured   1041500
    ## 619              Michigan 2013     Employer   5388000
    ## 620              Michigan 2013     Medicare   1384300
    ## 621              Michigan 2013 Other.Public     68300
    ## 622              Michigan 2013    Non.Group    317800
    ## 623              Michigan 2013        Total   9892400
    ## 624              Michigan 2014     Medicaid   2007900
    ## 625              Michigan 2014    Uninsured    697900
    ## 626              Michigan 2014     Employer   5176600
    ## 627              Michigan 2014     Medicare   1414500
    ## 628              Michigan 2014 Other.Public     53100
    ## 629              Michigan 2014    Non.Group    556400
    ## 630              Michigan 2014        Total   9906400
    ## 631              Michigan 2015     Employer   5185900
    ## 632              Michigan 2015    Non.Group    561800
    ## 633              Michigan 2015     Medicaid   1912700
    ## 634              Michigan 2015     Medicare   1541800
    ## 635              Michigan 2015 Other.Public       N/A
    ## 636              Michigan 2015    Uninsured    571200
    ## 637              Michigan 2015        Total   9862100
    ## 638              Michigan 2016     Employer   5046700
    ## 639              Michigan 2016    Non.Group    456400
    ## 640              Michigan 2016     Medicaid   2168900
    ## 641              Michigan 2016     Medicare   1522400
    ## 642              Michigan 2016 Other.Public     72500
    ## 643              Michigan 2016    Uninsured    626300
    ## 644              Michigan 2016        Total   9893200
    ## 645             Minnesota 2013    Uninsured    370800
    ## 646             Minnesota 2013     Medicaid    730900
    ## 647             Minnesota 2013     Medicare    690700
    ## 648             Minnesota 2013     Employer   3234700
    ## 649             Minnesota 2013    Non.Group    305700
    ## 650             Minnesota 2013        Total   5381300
    ## 651             Minnesota 2013 Other.Public     48500
    ## 652             Minnesota 2014    Uninsured    365300
    ## 653             Minnesota 2014     Medicaid    803700
    ## 654             Minnesota 2014     Medicare    713500
    ## 655             Minnesota 2014     Employer   3119000
    ## 656             Minnesota 2014    Non.Group    366600
    ## 657             Minnesota 2014        Total   5418500
    ## 658             Minnesota 2014 Other.Public     50500
    ## 659             Minnesota 2015     Employer   3041800
    ## 660             Minnesota 2015    Non.Group    423600
    ## 661             Minnesota 2015     Medicaid    785700
    ## 662             Minnesota 2015     Medicare    824900
    ## 663             Minnesota 2015 Other.Public     52800
    ## 664             Minnesota 2015    Uninsured    334100
    ## 665             Minnesota 2015        Total   5463000
    ## 666             Minnesota 2016     Employer   3010000
    ## 667             Minnesota 2016    Non.Group    444500
    ## 668             Minnesota 2016     Medicaid    787500
    ## 669             Minnesota 2016     Medicare    821100
    ## 670             Minnesota 2016 Other.Public       N/A
    ## 671             Minnesota 2016    Uninsured    321200
    ## 672             Minnesota 2016        Total   5436600
    ## 673           Mississippi 2013    Uninsured    414600
    ## 674           Mississippi 2013     Medicaid    646200
    ## 675           Mississippi 2013        Total   2903600
    ## 676           Mississippi 2013     Medicare    421700
    ## 677           Mississippi 2013     Employer   1270000
    ## 678           Mississippi 2013 Other.Public     62500
    ## 679           Mississippi 2013    Non.Group     88600
    ## 680           Mississippi 2014    Uninsured    364100
    ## 681           Mississippi 2014     Medicaid    762900
    ## 682           Mississippi 2014        Total   2965300
    ## 683           Mississippi 2014     Medicare    390500
    ## 684           Mississippi 2014     Employer   1192400
    ## 685           Mississippi 2014 Other.Public    108800
    ## 686           Mississippi 2014    Non.Group    146500
    ## 687           Mississippi 2015     Employer   1207700
    ## 688           Mississippi 2015    Non.Group    160100
    ## 689           Mississippi 2015     Medicaid    689900
    ## 690           Mississippi 2015     Medicare    428400
    ## 691           Mississippi 2015 Other.Public     82100
    ## 692           Mississippi 2015    Uninsured    380000
    ## 693           Mississippi 2015        Total   2948600
    ## 694           Mississippi 2016     Employer   1233800
    ## 695           Mississippi 2016    Non.Group    159300
    ## 696           Mississippi 2016     Medicaid    696400
    ## 697           Mississippi 2016     Medicare    418600
    ## 698           Mississippi 2016 Other.Public     88100
    ## 699           Mississippi 2016    Uninsured    351900
    ## 700           Mississippi 2016        Total   2948100
    ## 701              Missouri 2013    Uninsured    655800
    ## 702              Missouri 2013     Medicaid    863800
    ## 703              Missouri 2013        Total   5969400
    ## 704              Missouri 2013     Medicare   1030300
    ## 705              Missouri 2013     Employer   3026400
    ## 706              Missouri 2013 Other.Public     70000
    ## 707              Missouri 2013    Non.Group    323100
    ## 708              Missouri 2014    Uninsured    525100
    ## 709              Missouri 2014     Medicaid    879300
    ## 710              Missouri 2014        Total   5961300
    ## 711              Missouri 2014     Medicare    894600
    ## 712              Missouri 2014     Employer   3198400
    ## 713              Missouri 2014 Other.Public     93300
    ## 714              Missouri 2014    Non.Group    370700
    ## 715              Missouri 2015     Employer   3345700
    ## 716              Missouri 2015    Non.Group    398600
    ## 717              Missouri 2015     Medicaid    757600
    ## 718              Missouri 2015     Medicare    865900
    ## 719              Missouri 2015 Other.Public     67900
    ## 720              Missouri 2015    Uninsured    527000
    ## 721              Missouri 2015        Total   5962700
    ## 722              Missouri 2016     Employer   3021200
    ## 723              Missouri 2016    Non.Group    467500
    ## 724              Missouri 2016     Medicaid    871500
    ## 725              Missouri 2016     Medicare    989100
    ## 726              Missouri 2016 Other.Public     53000
    ## 727              Missouri 2016    Uninsured    486300
    ## 728              Missouri 2016        Total   5888700
    ## 729               Montana 2013     Medicaid    135700
    ## 730               Montana 2013     Employer    440700
    ## 731               Montana 2013    Uninsured    158000
    ## 732               Montana 2013        Total    993200
    ## 733               Montana 2013     Medicare    167600
    ## 734               Montana 2013 Other.Public     38100
    ## 735               Montana 2013    Non.Group     53000
    ## 736               Montana 2014     Medicaid    135400
    ## 737               Montana 2014     Employer    478200
    ## 738               Montana 2014    Uninsured    126600
    ## 739               Montana 2014        Total   1009100
    ## 740               Montana 2014     Medicare    151100
    ## 741               Montana 2014 Other.Public     31900
    ## 742               Montana 2014    Non.Group     85900
    ## 743               Montana 2015     Employer    469000
    ## 744               Montana 2015    Non.Group     65500
    ## 745               Montana 2015     Medicaid    165600
    ## 746               Montana 2015     Medicare    177500
    ## 747               Montana 2015 Other.Public     37600
    ## 748               Montana 2015    Uninsured    102800
    ## 749               Montana 2015        Total   1018100
    ## 750               Montana 2016     Employer    448700
    ## 751               Montana 2016    Non.Group     72100
    ## 752               Montana 2016     Medicaid    222700
    ## 753               Montana 2016     Medicare    183700
    ## 754               Montana 2016 Other.Public     35200
    ## 755               Montana 2016    Uninsured     72700
    ## 756               Montana 2016        Total   1035500
    ## 757              Nebraska 2013     Employer   1050800
    ## 758              Nebraska 2013    Uninsured    171600
    ## 759              Nebraska 2013     Medicaid    226800
    ## 760              Nebraska 2013     Medicare    241100
    ## 761              Nebraska 2013        Total   1852000
    ## 762              Nebraska 2013    Non.Group    136600
    ## 763              Nebraska 2013 Other.Public     25100
    ## 764              Nebraska 2014     Employer    999000
    ## 765              Nebraska 2014    Uninsured    180200
    ## 766              Nebraska 2014     Medicaid    266100
    ## 767              Nebraska 2014     Medicare    258500
    ## 768              Nebraska 2014        Total   1881000
    ## 769              Nebraska 2014    Non.Group    128100
    ## 770              Nebraska 2014 Other.Public     49200
    ## 771              Nebraska 2015     Employer   1029100
    ## 772              Nebraska 2015    Non.Group    133800
    ## 773              Nebraska 2015     Medicaid    249600
    ## 774              Nebraska 2015     Medicare    241400
    ## 775              Nebraska 2015 Other.Public       N/A
    ## 776              Nebraska 2015    Uninsured    157900
    ## 777              Nebraska 2015        Total   1859800
    ## 778              Nebraska 2016     Employer   1039100
    ## 779              Nebraska 2016    Non.Group    140200
    ## 780              Nebraska 2016     Medicaid    244500
    ## 781              Nebraska 2016     Medicare    280900
    ## 782              Nebraska 2016 Other.Public     41300
    ## 783              Nebraska 2016    Uninsured    130100
    ## 784              Nebraska 2016        Total   1876100
    ## 785                Nevada 2013        Total   2767100
    ## 786                Nevada 2013    Uninsured    534700
    ## 787                Nevada 2013     Medicaid    357900
    ## 788                Nevada 2013     Employer   1345500
    ## 789                Nevada 2013 Other.Public     80500
    ## 790                Nevada 2013     Medicare    333500
    ## 791                Nevada 2013    Non.Group    115000
    ## 792                Nevada 2014        Total   2823400
    ## 793                Nevada 2014    Uninsured    356800
    ## 794                Nevada 2014     Medicaid    494600
    ## 795                Nevada 2014     Employer   1324700
    ## 796                Nevada 2014 Other.Public    138000
    ## 797                Nevada 2014     Medicare    368300
    ## 798                Nevada 2014    Non.Group    141100
    ## 799                Nevada 2015     Employer   1328200
    ## 800                Nevada 2015    Non.Group    200200
    ## 801                Nevada 2015     Medicaid    489300
    ## 802                Nevada 2015     Medicare    381400
    ## 803                Nevada 2015 Other.Public    143500
    ## 804                Nevada 2015    Uninsured    324800
    ## 805                Nevada 2015        Total   2867400
    ## 806                Nevada 2016     Employer   1458200
    ## 807                Nevada 2016    Non.Group    152000
    ## 808                Nevada 2016     Medicaid    515500
    ## 809                Nevada 2016     Medicare    454700
    ## 810                Nevada 2016 Other.Public    107200
    ## 811                Nevada 2016    Uninsured    255600
    ## 812                Nevada 2016        Total   2943200
    ## 813         New Hampshire 2013     Employer    778400
    ## 814         New Hampshire 2013    Non.Group     45500
    ## 815         New Hampshire 2013    Uninsured    151000
    ## 816         New Hampshire 2013     Medicaid    142300
    ## 817         New Hampshire 2013        Total   1318500
    ## 818         New Hampshire 2013     Medicare    186100
    ## 819         New Hampshire 2013 Other.Public     15100
    ## 820         New Hampshire 2014     Employer    784300
    ## 821         New Hampshire 2014    Non.Group     74700
    ## 822         New Hampshire 2014    Uninsured     95000
    ## 823         New Hampshire 2014     Medicaid    158100
    ## 824         New Hampshire 2014        Total   1319700
    ## 825         New Hampshire 2014     Medicare    198600
    ## 826         New Hampshire 2014 Other.Public       N/A
    ## 827         New Hampshire 2015     Employer    794600
    ## 828         New Hampshire 2015    Non.Group     65200
    ## 829         New Hampshire 2015     Medicaid    165300
    ## 830         New Hampshire 2015     Medicare    184500
    ## 831         New Hampshire 2015 Other.Public       N/A
    ## 832         New Hampshire 2015    Uninsured     68100
    ## 833         New Hampshire 2015        Total   1292800
    ## 834         New Hampshire 2016     Employer    761800
    ## 835         New Hampshire 2016    Non.Group     73700
    ## 836         New Hampshire 2016     Medicaid    165300
    ## 837         New Hampshire 2016     Medicare    209300
    ## 838         New Hampshire 2016 Other.Public     21700
    ## 839         New Hampshire 2016    Uninsured     85800
    ## 840         New Hampshire 2016        Total   1318100
    ## 841            New Jersey 2013    Non.Group    293600
    ## 842            New Jersey 2013     Medicaid   1262900
    ## 843            New Jersey 2013     Employer   5060100
    ## 844            New Jersey 2013        Total   8807400
    ## 845            New Jersey 2013    Uninsured   1034500
    ## 846            New Jersey 2013     Medicare   1112600
    ## 847            New Jersey 2013 Other.Public       N/A
    ## 848            New Jersey 2014    Non.Group    381500
    ## 849            New Jersey 2014     Medicaid   1505400
    ## 850            New Jersey 2014     Employer   4877600
    ## 851            New Jersey 2014        Total   8939300
    ## 852            New Jersey 2014    Uninsured    954500
    ## 853            New Jersey 2014     Medicare   1130900
    ## 854            New Jersey 2014 Other.Public     89500
    ## 855            New Jersey 2015     Employer   4909400
    ## 856            New Jersey 2015    Non.Group    512800
    ## 857            New Jersey 2015     Medicaid   1614300
    ## 858            New Jersey 2015     Medicare   1136500
    ## 859            New Jersey 2015 Other.Public       N/A
    ## 860            New Jersey 2015    Uninsured    699400
    ## 861            New Jersey 2015        Total   8941600
    ## 862            New Jersey 2016     Employer   4838400
    ## 863            New Jersey 2016    Non.Group    545300
    ## 864            New Jersey 2016     Medicaid   1492700
    ## 865            New Jersey 2016     Medicare   1240700
    ## 866            New Jersey 2016 Other.Public       N/A
    ## 867            New Jersey 2016    Uninsured    696200
    ## 868            New Jersey 2016        Total   8851500
    ## 869            New Mexico 2013     Medicaid    470000
    ## 870            New Mexico 2013     Employer    797400
    ## 871            New Mexico 2013    Uninsured    342200
    ## 872            New Mexico 2013    Non.Group     82400
    ## 873            New Mexico 2013     Medicare    334500
    ## 874            New Mexico 2013        Total   2081300
    ## 875            New Mexico 2013 Other.Public     54800
    ## 876            New Mexico 2014     Medicaid    579800
    ## 877            New Mexico 2014     Employer    751700
    ## 878            New Mexico 2014    Uninsured    236900
    ## 879            New Mexico 2014    Non.Group     94800
    ## 880            New Mexico 2014     Medicare    306100
    ## 881            New Mexico 2014        Total   2035200
    ## 882            New Mexico 2014 Other.Public     66000
    ## 883            New Mexico 2015     Employer    760100
    ## 884            New Mexico 2015    Non.Group     98200
    ## 885            New Mexico 2015     Medicaid    551000
    ## 886            New Mexico 2015     Medicare    313200
    ## 887            New Mexico 2015 Other.Public     66500
    ## 888            New Mexico 2015    Uninsured    252100
    ## 889            New Mexico 2015        Total   2041000
    ## 890            New Mexico 2016     Employer    740300
    ## 891            New Mexico 2016    Non.Group    102800
    ## 892            New Mexico 2016     Medicaid    641300
    ## 893            New Mexico 2016     Medicare    281200
    ## 894            New Mexico 2016 Other.Public     49500
    ## 895            New Mexico 2016    Uninsured    230000
    ## 896            New Mexico 2016        Total   2045000
    ## 897              New York 2013     Medicaid   4553100
    ## 898              New York 2013     Medicare   2461600
    ## 899              New York 2013     Employer   9616000
    ## 900              New York 2013        Total  19471100
    ## 901              New York 2013    Uninsured   1885100
    ## 902              New York 2013 Other.Public    164700
    ## 903              New York 2013    Non.Group    790500
    ## 904              New York 2014     Medicaid   4957600
    ## 905              New York 2014     Medicare   2366700
    ## 906              New York 2014     Employer   9575500
    ## 907              New York 2014        Total  19679400
    ## 908              New York 2014    Uninsured   1520800
    ## 909              New York 2014 Other.Public    110500
    ## 910              New York 2014    Non.Group   1148400
    ## 911              New York 2015     Employer   9607700
    ## 912              New York 2015    Non.Group   1440600
    ## 913              New York 2015     Medicaid   4658100
    ## 914              New York 2015     Medicare   2584300
    ## 915              New York 2015 Other.Public       N/A
    ## 916              New York 2015    Uninsured   1264200
    ## 917              New York 2015        Total  19695000
    ## 918              New York 2016     Employer   9767500
    ## 919              New York 2016    Non.Group   1294200
    ## 920              New York 2016     Medicaid   4621700
    ## 921              New York 2016     Medicare   2517200
    ## 922              New York 2016 Other.Public    183400
    ## 923              New York 2016    Uninsured   1094400
    ## 924              New York 2016        Total  19482300
    ## 925        North Carolina 2013     Medicaid   1717400
    ## 926        North Carolina 2013     Medicare   1352600
    ## 927        North Carolina 2013     Employer   4295500
    ## 928        North Carolina 2013    Non.Group    529800
    ## 929        North Carolina 2013 Other.Public    318100
    ## 930        North Carolina 2013        Total   9652400
    ## 931        North Carolina 2013    Uninsured   1439100
    ## 932        North Carolina 2014     Medicaid   1760200
    ## 933        North Carolina 2014     Medicare   1348900
    ## 934        North Carolina 2014     Employer   4689700
    ## 935        North Carolina 2014    Non.Group    570200
    ## 936        North Carolina 2014 Other.Public    307200
    ## 937        North Carolina 2014        Total   9835800
    ## 938        North Carolina 2014    Uninsured   1159500
    ## 939        North Carolina 2015     Employer   4709100
    ## 940        North Carolina 2015    Non.Group    710400
    ## 941        North Carolina 2015     Medicaid   1787300
    ## 942        North Carolina 2015     Medicare   1288500
    ## 943        North Carolina 2015 Other.Public       N/A
    ## 944        North Carolina 2015    Uninsured   1094800
    ## 945        North Carolina 2015        Total   9902000
    ## 946        North Carolina 2016     Employer   4570100
    ## 947        North Carolina 2016    Non.Group    863500
    ## 948        North Carolina 2016     Medicaid   1787900
    ## 949        North Carolina 2016     Medicare   1478000
    ## 950        North Carolina 2016 Other.Public    281800
    ## 951        North Carolina 2016    Uninsured   1087000
    ## 952        North Carolina 2016        Total  10068300
    ## 953          North Dakota 2013 Other.Public       N/A
    ## 954          North Dakota 2013     Medicaid     69200
    ## 955          North Dakota 2013    Non.Group     60700
    ## 956          North Dakota 2013     Medicare     80900
    ## 957          North Dakota 2013     Employer    413100
    ## 958          North Dakota 2013    Uninsured     77200
    ## 959          North Dakota 2013        Total    714500
    ## 960          North Dakota 2014 Other.Public     17300
    ## 961          North Dakota 2014     Medicaid     65400
    ## 962          North Dakota 2014    Non.Group     58800
    ## 963          North Dakota 2014     Medicare     94800
    ## 964          North Dakota 2014     Employer    433000
    ## 965          North Dakota 2014    Uninsured     64200
    ## 966          North Dakota 2014        Total    733400
    ## 967          North Dakota 2015     Employer    436500
    ## 968          North Dakota 2015    Non.Group     61100
    ## 969          North Dakota 2015     Medicaid     79700
    ## 970          North Dakota 2015     Medicare    104600
    ## 971          North Dakota 2015 Other.Public     19700
    ## 972          North Dakota 2015    Uninsured     61800
    ## 973          North Dakota 2015        Total    763400
    ## 974          North Dakota 2016     Employer    415000
    ## 975          North Dakota 2016    Non.Group     58500
    ## 976          North Dakota 2016     Medicaid     91300
    ## 977          North Dakota 2016     Medicare    100200
    ## 978          North Dakota 2016 Other.Public     26700
    ## 979          North Dakota 2016    Uninsured     57800
    ## 980          North Dakota 2016        Total    749500
    ## 981                  Ohio 2013 Other.Public    172500
    ## 982                  Ohio 2013     Medicaid   1857000
    ## 983                  Ohio 2013     Employer   5883900
    ## 984                  Ohio 2013    Non.Group    396500
    ## 985                  Ohio 2013    Uninsured   1358100
    ## 986                  Ohio 2013     Medicare   1745600
    ## 987                  Ohio 2013        Total  11413600
    ## 988                  Ohio 2014 Other.Public    116400
    ## 989                  Ohio 2014     Medicaid   2471700
    ## 990                  Ohio 2014     Employer   5777000
    ## 991                  Ohio 2014    Non.Group    456800
    ## 992                  Ohio 2014    Uninsured    857700
    ## 993                  Ohio 2014     Medicare   1856000
    ## 994                  Ohio 2014        Total  11535600
    ## 995                  Ohio 2015     Employer   5974700
    ## 996                  Ohio 2015    Non.Group    603700
    ## 997                  Ohio 2015     Medicaid   2383300
    ## 998                  Ohio 2015     Medicare   1681500
    ## 999                  Ohio 2015 Other.Public    126400
    ## 1000                 Ohio 2015    Uninsured    681400
    ## 1001                 Ohio 2015        Total  11450900
    ## 1002                 Ohio 2016     Employer   5847600
    ## 1003                 Ohio 2016    Non.Group    627500
    ## 1004                 Ohio 2016     Medicaid   2506900
    ## 1005                 Ohio 2016     Medicare   1753800
    ## 1006                 Ohio 2016 Other.Public     95000
    ## 1007                 Ohio 2016    Uninsured    638000
    ## 1008                 Ohio 2016        Total  11468700
    ## 1009             Oklahoma 2013 Other.Public       N/A
    ## 1010             Oklahoma 2013    Uninsured    574200
    ## 1011             Oklahoma 2013     Medicaid    699600
    ## 1012             Oklahoma 2013     Medicare    516000
    ## 1013             Oklahoma 2013     Employer   1679100
    ## 1014             Oklahoma 2013        Total   3709400
    ## 1015             Oklahoma 2013    Non.Group    133600
    ## 1016             Oklahoma 2014 Other.Public     76100
    ## 1017             Oklahoma 2014    Uninsured    583400
    ## 1018             Oklahoma 2014     Medicaid    648300
    ## 1019             Oklahoma 2014     Medicare    538000
    ## 1020             Oklahoma 2014     Employer   1692100
    ## 1021             Oklahoma 2014        Total   3741700
    ## 1022             Oklahoma 2014    Non.Group    203900
    ## 1023             Oklahoma 2015     Employer   1782600
    ## 1024             Oklahoma 2015    Non.Group    262300
    ## 1025             Oklahoma 2015     Medicaid    671500
    ## 1026             Oklahoma 2015     Medicare    550200
    ## 1027             Oklahoma 2015 Other.Public    122500
    ## 1028             Oklahoma 2015    Uninsured    513900
    ## 1029             Oklahoma 2015        Total   3902900
    ## 1030             Oklahoma 2016     Employer   1885900
    ## 1031             Oklahoma 2016    Non.Group    192400
    ## 1032             Oklahoma 2016     Medicaid    688200
    ## 1033             Oklahoma 2016     Medicare    622800
    ## 1034             Oklahoma 2016 Other.Public    120000
    ## 1035             Oklahoma 2016    Uninsured    411800
    ## 1036             Oklahoma 2016        Total   3921100
    ## 1037               Oregon 2013 Other.Public     74800
    ## 1038               Oregon 2013    Uninsured    481400
    ## 1039               Oregon 2013     Medicaid    686300
    ## 1040               Oregon 2013    Non.Group    203700
    ## 1041               Oregon 2013        Total   3960300
    ## 1042               Oregon 2013     Medicare    597200
    ## 1043               Oregon 2013     Employer   1915000
    ## 1044               Oregon 2014 Other.Public     59700
    ## 1045               Oregon 2014    Uninsured    310800
    ## 1046               Oregon 2014     Medicaid    825400
    ## 1047               Oregon 2014    Non.Group    290300
    ## 1048               Oregon 2014        Total   3962300
    ## 1049               Oregon 2014     Medicare    635700
    ## 1050               Oregon 2014     Employer   1840400
    ## 1051               Oregon 2015     Employer   1865500
    ## 1052               Oregon 2015    Non.Group    272400
    ## 1053               Oregon 2015     Medicaid    964200
    ## 1054               Oregon 2015     Medicare    578600
    ## 1055               Oregon 2015 Other.Public     64800
    ## 1056               Oregon 2015    Uninsured    287300
    ## 1057               Oregon 2015        Total   4032800
    ## 1058               Oregon 2016     Employer   1910600
    ## 1059               Oregon 2016    Non.Group    286000
    ## 1060               Oregon 2016     Medicaid   1009900
    ## 1061               Oregon 2016     Medicare    650500
    ## 1062               Oregon 2016 Other.Public     54800
    ## 1063               Oregon 2016    Uninsured    218400
    ## 1064               Oregon 2016        Total   4130200
    ## 1065         Pennsylvania 2013 Other.Public     94300
    ## 1066         Pennsylvania 2013        Total  12772600
    ## 1067         Pennsylvania 2013    Uninsured   1255700
    ## 1068         Pennsylvania 2013     Medicaid   1968000
    ## 1069         Pennsylvania 2013    Non.Group    516100
    ## 1070         Pennsylvania 2013     Medicare   1910800
    ## 1071         Pennsylvania 2013     Employer   7027800
    ## 1072         Pennsylvania 2014 Other.Public     85100
    ## 1073         Pennsylvania 2014        Total  12627100
    ## 1074         Pennsylvania 2014    Uninsured   1020800
    ## 1075         Pennsylvania 2014     Medicaid   2127200
    ## 1076         Pennsylvania 2014    Non.Group    793200
    ## 1077         Pennsylvania 2014     Medicare   1948700
    ## 1078         Pennsylvania 2014     Employer   6652200
    ## 1079         Pennsylvania 2015     Employer   6870000
    ## 1080         Pennsylvania 2015    Non.Group    673000
    ## 1081         Pennsylvania 2015     Medicaid   2256200
    ## 1082         Pennsylvania 2015     Medicare   1975500
    ## 1083         Pennsylvania 2015 Other.Public     93200
    ## 1084         Pennsylvania 2015    Uninsured    728000
    ## 1085         Pennsylvania 2015        Total  12595900
    ## 1086         Pennsylvania 2016     Employer   6663200
    ## 1087         Pennsylvania 2016    Non.Group    721300
    ## 1088         Pennsylvania 2016     Medicaid   2379500
    ## 1089         Pennsylvania 2016     Medicare   2040900
    ## 1090         Pennsylvania 2016 Other.Public    151500
    ## 1091         Pennsylvania 2016    Uninsured    601500
    ## 1092         Pennsylvania 2016        Total  12557900
    ## 1093         Rhode Island 2013 Other.Public     12100
    ## 1094         Rhode Island 2013     Medicaid    166100
    ## 1095         Rhode Island 2013     Employer    564100
    ## 1096         Rhode Island 2013    Uninsured     95300
    ## 1097         Rhode Island 2013        Total   1044300
    ## 1098         Rhode Island 2013     Medicare    143100
    ## 1099         Rhode Island 2013    Non.Group     63700
    ## 1100         Rhode Island 2014 Other.Public       N/A
    ## 1101         Rhode Island 2014     Medicaid    185300
    ## 1102         Rhode Island 2014     Employer    562100
    ## 1103         Rhode Island 2014    Uninsured     56800
    ## 1104         Rhode Island 2014        Total   1048200
    ## 1105         Rhode Island 2014     Medicare    136800
    ## 1106         Rhode Island 2014    Non.Group     99500
    ## 1107         Rhode Island 2015     Employer    593700
    ## 1108         Rhode Island 2015    Non.Group     72100
    ## 1109         Rhode Island 2015     Medicaid    176500
    ## 1110         Rhode Island 2015     Medicare    135100
    ## 1111         Rhode Island 2015 Other.Public       N/A
    ## 1112         Rhode Island 2015    Uninsured     49500
    ## 1113         Rhode Island 2015        Total   1044800
    ## 1114         Rhode Island 2016     Employer    535000
    ## 1115         Rhode Island 2016    Non.Group     83700
    ## 1116         Rhode Island 2016     Medicaid    214100
    ## 1117         Rhode Island 2016     Medicare    156800
    ## 1118         Rhode Island 2016 Other.Public       N/A
    ## 1119         Rhode Island 2016    Uninsured     50500
    ## 1120         Rhode Island 2016        Total   1054300
    ## 1121       South Carolina 2013    Uninsured    758000
    ## 1122       South Carolina 2013 Other.Public     90000
    ## 1123       South Carolina 2013     Medicare    757400
    ## 1124       South Carolina 2013     Medicaid    814900
    ## 1125       South Carolina 2013     Employer   2136200
    ## 1126       South Carolina 2013        Total   4688800
    ## 1127       South Carolina 2013    Non.Group    132300
    ## 1128       South Carolina 2014    Uninsured    614100
    ## 1129       South Carolina 2014 Other.Public     93100
    ## 1130       South Carolina 2014     Medicare    732400
    ## 1131       South Carolina 2014     Medicaid    968100
    ## 1132       South Carolina 2014     Employer   2179100
    ## 1133       South Carolina 2014        Total   4764300
    ## 1134       South Carolina 2014    Non.Group    177500
    ## 1135       South Carolina 2015     Employer   2201700
    ## 1136       South Carolina 2015    Non.Group    278100
    ## 1137       South Carolina 2015     Medicaid    897900
    ## 1138       South Carolina 2015     Medicare    790800
    ## 1139       South Carolina 2015 Other.Public    106000
    ## 1140       South Carolina 2015    Uninsured    518900
    ## 1141       South Carolina 2015        Total   4794700
    ## 1142       South Carolina 2016     Employer   2257300
    ## 1143       South Carolina 2016    Non.Group    324900
    ## 1144       South Carolina 2016     Medicaid    916600
    ## 1145       South Carolina 2016     Medicare    766800
    ## 1146       South Carolina 2016 Other.Public    194600
    ## 1147       South Carolina 2016    Uninsured    448200
    ## 1148       South Carolina 2016        Total   4908400
    ## 1149         South Dakota 2013        Total    833400
    ## 1150         South Dakota 2013 Other.Public     23200
    ## 1151         South Dakota 2013     Employer    444100
    ## 1152         South Dakota 2013    Uninsured     83100
    ## 1153         South Dakota 2013     Medicaid     98400
    ## 1154         South Dakota 2013    Non.Group     64000
    ## 1155         South Dakota 2013     Medicare    120700
    ## 1156         South Dakota 2014        Total    846800
    ## 1157         South Dakota 2014 Other.Public     19000
    ## 1158         South Dakota 2014     Employer    463300
    ## 1159         South Dakota 2014    Uninsured     77600
    ## 1160         South Dakota 2014     Medicaid    104100
    ## 1161         South Dakota 2014    Non.Group     60000
    ## 1162         South Dakota 2014     Medicare    122800
    ## 1163         South Dakota 2015     Employer    447500
    ## 1164         South Dakota 2015    Non.Group     73200
    ## 1165         South Dakota 2015     Medicaid    117000
    ## 1166         South Dakota 2015     Medicare    115500
    ## 1167         South Dakota 2015 Other.Public       N/A
    ## 1168         South Dakota 2015    Uninsured     78900
    ## 1169         South Dakota 2015        Total    848400
    ## 1170         South Dakota 2016     Employer    416500
    ## 1171         South Dakota 2016    Non.Group     82100
    ## 1172         South Dakota 2016     Medicaid    126900
    ## 1173         South Dakota 2016     Medicare    143200
    ## 1174         South Dakota 2016 Other.Public     19700
    ## 1175         South Dakota 2016    Uninsured     67000
    ## 1176         South Dakota 2016        Total    855400
    ## 1177            Tennessee 2013     Employer   2895200
    ## 1178            Tennessee 2013    Non.Group    218200
    ## 1179            Tennessee 2013    Uninsured    844200
    ## 1180            Tennessee 2013 Other.Public       N/A
    ## 1181            Tennessee 2013        Total   6400200
    ## 1182            Tennessee 2013     Medicaid   1244700
    ## 1183            Tennessee 2013     Medicare    984400
    ## 1184            Tennessee 2014     Employer   3230900
    ## 1185            Tennessee 2014    Non.Group    382800
    ## 1186            Tennessee 2014    Uninsured    625000
    ## 1187            Tennessee 2014 Other.Public       N/A
    ## 1188            Tennessee 2014        Total   6502000
    ## 1189            Tennessee 2014     Medicaid   1065800
    ## 1190            Tennessee 2014     Medicare    988800
    ## 1191            Tennessee 2015     Employer   2969100
    ## 1192            Tennessee 2015    Non.Group    415400
    ## 1193            Tennessee 2015     Medicaid   1249600
    ## 1194            Tennessee 2015     Medicare   1052200
    ## 1195            Tennessee 2015 Other.Public       N/A
    ## 1196            Tennessee 2015    Uninsured    718100
    ## 1197            Tennessee 2015        Total   6616500
    ## 1198            Tennessee 2016     Employer   3072000
    ## 1199            Tennessee 2016    Non.Group    370700
    ## 1200            Tennessee 2016     Medicaid   1337600
    ## 1201            Tennessee 2016     Medicare    985500
    ## 1202            Tennessee 2016 Other.Public       N/A
    ## 1203            Tennessee 2016    Uninsured    744800
    ## 1204            Tennessee 2016        Total   6674100
    ## 1205                Texas 2013 Other.Public    665900
    ## 1206                Texas 2013    Non.Group   1036800
    ## 1207                Texas 2013     Medicaid   4312300
    ## 1208                Texas 2013     Employer  12283600
    ## 1209                Texas 2013    Uninsured   5405700
    ## 1210                Texas 2013        Total  26383500
    ## 1211                Texas 2013     Medicare   2679300
    ## 1212                Texas 2014 Other.Public    509200
    ## 1213                Texas 2014    Non.Group   1545300
    ## 1214                Texas 2014     Medicaid   4725300
    ## 1215                Texas 2014     Employer  12620500
    ## 1216                Texas 2014    Uninsured   4499500
    ## 1217                Texas 2014        Total  26687400
    ## 1218                Texas 2014     Medicare   2787600
    ## 1219                Texas 2015     Employer  13119300
    ## 1220                Texas 2015    Non.Group   1815300
    ## 1221                Texas 2015     Medicaid   4380400
    ## 1222                Texas 2015     Medicare   3059800
    ## 1223                Texas 2015 Other.Public    726000
    ## 1224                Texas 2015    Uninsured   4333600
    ## 1225                Texas 2015        Total  27434400
    ## 1226                Texas 2016     Employer  13607200
    ## 1227                Texas 2016    Non.Group   1662100
    ## 1228                Texas 2016     Medicaid   4513800
    ## 1229                Texas 2016     Medicare   3083900
    ## 1230                Texas 2016 Other.Public    565100
    ## 1231                Texas 2016    Uninsured   4245000
    ## 1232                Texas 2016        Total  27677200
    ## 1233        United States 2013     Employer 155696900
    ## 1234        United States 2013    Uninsured  41795100
    ## 1235        United States 2013 Other.Public   6295400
    ## 1236        United States 2013        Total 313401200
    ## 1237        United States 2013     Medicare  40876300
    ## 1238        United States 2013     Medicaid  54919100
    ## 1239        United States 2013    Non.Group  13816000
    ## 1240        United States 2014     Employer 154347500
    ## 1241        United States 2014    Uninsured  32967500
    ## 1242        United States 2014 Other.Public   5985000
    ## 1243        United States 2014        Total 316159900
    ## 1244        United States 2014     Medicare  41896500
    ## 1245        United States 2014     Medicaid  61650400
    ## 1246        United States 2014    Non.Group  19313000
    ## 1247        United States 2015     Employer 155965800
    ## 1248        United States 2015    Non.Group  21816500
    ## 1249        United States 2015     Medicaid  62384500
    ## 1250        United States 2015     Medicare  43308400
    ## 1251        United States 2015 Other.Public   6422300
    ## 1252        United States 2015    Uninsured  28965900
    ## 1253        United States 2015        Total 318868500
    ## 1254        United States 2016     Employer 157381500
    ## 1255        United States 2016    Non.Group  21884400
    ## 1256        United States 2016     Medicaid  62303400
    ## 1257        United States 2016     Medicare  44550200
    ## 1258        United States 2016 Other.Public   6192200
    ## 1259        United States 2016    Uninsured  28051900
    ## 1260        United States 2016        Total 320372000
    ## 1261                 Utah 2013     Medicaid    346600
    ## 1262                 Utah 2013    Uninsured    359700
    ## 1263                 Utah 2013 Other.Public     39500
    ## 1264                 Utah 2013     Medicare    269400
    ## 1265                 Utah 2013        Total   2893500
    ## 1266                 Utah 2013    Non.Group    176200
    ## 1267                 Utah 2013     Employer   1702000
    ## 1268                 Utah 2014     Medicaid    304600
    ## 1269                 Utah 2014    Uninsured    339600
    ## 1270                 Utah 2014 Other.Public     36400
    ## 1271                 Utah 2014     Medicare    296300
    ## 1272                 Utah 2014        Total   2929400
    ## 1273                 Utah 2014    Non.Group    223600
    ## 1274                 Utah 2014     Employer   1728900
    ## 1275                 Utah 2015     Employer   1779500
    ## 1276                 Utah 2015    Non.Group    209100
    ## 1277                 Utah 2015     Medicaid    357200
    ## 1278                 Utah 2015     Medicare    307600
    ## 1279                 Utah 2015 Other.Public       N/A
    ## 1280                 Utah 2015    Uninsured    301800
    ## 1281                 Utah 2015        Total   3004500
    ## 1282                 Utah 2016     Employer   1854700
    ## 1283                 Utah 2016    Non.Group    203700
    ## 1284                 Utah 2016     Medicaid    308100
    ## 1285                 Utah 2016     Medicare    306300
    ## 1286                 Utah 2016 Other.Public       N/A
    ## 1287                 Utah 2016    Uninsured    373900
    ## 1288                 Utah 2016        Total   3079700
    ## 1289              Vermont 2013     Employer    317700
    ## 1290              Vermont 2013     Medicaid    123400
    ## 1291              Vermont 2013        Total    621400
    ## 1292              Vermont 2013    Uninsured     47700
    ## 1293              Vermont 2013 Other.Public      9900
    ## 1294              Vermont 2013     Medicare     96600
    ## 1295              Vermont 2013    Non.Group     26200
    ## 1296              Vermont 2014     Employer    321100
    ## 1297              Vermont 2014     Medicaid    127000
    ## 1298              Vermont 2014        Total    617000
    ## 1299              Vermont 2014    Uninsured     35600
    ## 1300              Vermont 2014 Other.Public      9900
    ## 1301              Vermont 2014     Medicare     80500
    ## 1302              Vermont 2014    Non.Group     42900
    ## 1303              Vermont 2015     Employer    309000
    ## 1304              Vermont 2015    Non.Group     44300
    ## 1305              Vermont 2015     Medicaid    120500
    ## 1306              Vermont 2015     Medicare     87900
    ## 1307              Vermont 2015 Other.Public     16500
    ## 1308              Vermont 2015    Uninsured     31500
    ## 1309              Vermont 2015        Total    609700
    ## 1310              Vermont 2016     Employer    292700
    ## 1311              Vermont 2016    Non.Group     55700
    ## 1312              Vermont 2016     Medicaid    124800
    ## 1313              Vermont 2016     Medicare    100600
    ## 1314              Vermont 2016 Other.Public     14600
    ## 1315              Vermont 2016    Uninsured     33900
    ## 1316              Vermont 2016        Total    622500
    ## 1317             Virginia 2013     Medicare    968000
    ## 1318             Virginia 2013     Employer   4661600
    ## 1319             Virginia 2013     Medicaid    773200
    ## 1320             Virginia 2013    Non.Group    364800
    ## 1321             Virginia 2013        Total   8204100
    ## 1322             Virginia 2013 Other.Public    492000
    ## 1323             Virginia 2013    Uninsured    944500
    ## 1324             Virginia 2014     Medicare   1070300
    ## 1325             Virginia 2014     Employer   4514000
    ## 1326             Virginia 2014     Medicaid    748300
    ## 1327             Virginia 2014    Non.Group    596000
    ## 1328             Virginia 2014        Total   8258800
    ## 1329             Virginia 2014 Other.Public    514300
    ## 1330             Virginia 2014    Uninsured    815900
    ## 1331             Virginia 2015     Employer   4332900
    ## 1332             Virginia 2015    Non.Group    638000
    ## 1333             Virginia 2015     Medicaid    939300
    ## 1334             Virginia 2015     Medicare   1168000
    ## 1335             Virginia 2015 Other.Public    375600
    ## 1336             Virginia 2015    Uninsured    763400
    ## 1337             Virginia 2015        Total   8217200
    ## 1338             Virginia 2016     Employer   4472900
    ## 1339             Virginia 2016    Non.Group    418700
    ## 1340             Virginia 2016     Medicaid    948100
    ## 1341             Virginia 2016     Medicare   1127600
    ## 1342             Virginia 2016 Other.Public    393900
    ## 1343             Virginia 2016    Uninsured    813900
    ## 1344             Virginia 2016        Total   8175000
    ## 1345           Washington 2013     Medicare    879000
    ## 1346           Washington 2013 Other.Public    301400
    ## 1347           Washington 2013     Employer   3541600
    ## 1348           Washington 2013     Medicaid   1026800
    ## 1349           Washington 2013    Non.Group    309000
    ## 1350           Washington 2013    Uninsured    808200
    ## 1351           Washington 2013        Total   6866000
    ## 1352           Washington 2014     Medicare    878600
    ## 1353           Washington 2014 Other.Public    126300
    ## 1354           Washington 2014     Employer   3400100
    ## 1355           Washington 2014     Medicaid   1560300
    ## 1356           Washington 2014    Non.Group    472400
    ## 1357           Washington 2014    Uninsured    647200
    ## 1358           Washington 2014        Total   7085000
    ## 1359           Washington 2015     Employer   3606400
    ## 1360           Washington 2015    Non.Group    413700
    ## 1361           Washington 2015     Medicaid   1548200
    ## 1362           Washington 2015     Medicare   1016400
    ## 1363           Washington 2015 Other.Public    116800
    ## 1364           Washington 2015    Uninsured    493200
    ## 1365           Washington 2015        Total   7194700
    ## 1366           Washington 2016     Employer   3593400
    ## 1367           Washington 2016    Non.Group    498500
    ## 1368           Washington 2016     Medicaid   1608200
    ## 1369           Washington 2016     Medicare    961800
    ## 1370           Washington 2016 Other.Public    121700
    ## 1371           Washington 2016    Uninsured    513800
    ## 1372           Washington 2016        Total   7297300
    ## 1373        West Virginia 2013 Other.Public       N/A
    ## 1374        West Virginia 2013     Medicaid    382500
    ## 1375        West Virginia 2013     Medicare    329400
    ## 1376        West Virginia 2013    Uninsured    213800
    ## 1377        West Virginia 2013    Non.Group     42600
    ## 1378        West Virginia 2013        Total   1822000
    ## 1379        West Virginia 2013     Employer    841300
    ## 1380        West Virginia 2014 Other.Public     24000
    ## 1381        West Virginia 2014     Medicaid    528800
    ## 1382        West Virginia 2014     Medicare    302900
    ## 1383        West Virginia 2014    Uninsured    118200
    ## 1384        West Virginia 2014    Non.Group     68700
    ## 1385        West Virginia 2014        Total   1825500
    ## 1386        West Virginia 2014     Employer    783000
    ## 1387        West Virginia 2015     Employer    722800
    ## 1388        West Virginia 2015    Non.Group     78700
    ## 1389        West Virginia 2015     Medicaid    513200
    ## 1390        West Virginia 2015     Medicare    340200
    ## 1391        West Virginia 2015 Other.Public     26500
    ## 1392        West Virginia 2015    Uninsured    116200
    ## 1393        West Virginia 2015        Total   1797500
    ## 1394        West Virginia 2016     Employer    788500
    ## 1395        West Virginia 2016    Non.Group     64000
    ## 1396        West Virginia 2016     Medicaid    470900
    ## 1397        West Virginia 2016     Medicare    332600
    ## 1398        West Virginia 2016 Other.Public     24900
    ## 1399        West Virginia 2016    Uninsured    133100
    ## 1400        West Virginia 2016        Total   1814100
    ## 1401            Wisconsin 2013 Other.Public     41200
    ## 1402            Wisconsin 2013    Uninsured    500300
    ## 1403            Wisconsin 2013     Medicaid    907600
    ## 1404            Wisconsin 2013     Employer   3154500
    ## 1405            Wisconsin 2013        Total   5641900
    ## 1406            Wisconsin 2013     Medicare    812900
    ## 1407            Wisconsin 2013    Non.Group    225300
    ## 1408            Wisconsin 2014 Other.Public       N/A
    ## 1409            Wisconsin 2014    Uninsured    420800
    ## 1410            Wisconsin 2014     Medicaid    956600
    ## 1411            Wisconsin 2014     Employer   3150100
    ## 1412            Wisconsin 2014        Total   5747200
    ## 1413            Wisconsin 2014     Medicare    827500
    ## 1414            Wisconsin 2014    Non.Group    348200
    ## 1415            Wisconsin 2015     Employer   3146300
    ## 1416            Wisconsin 2015    Non.Group    323000
    ## 1417            Wisconsin 2015     Medicaid   1002800
    ## 1418            Wisconsin 2015     Medicare    813600
    ## 1419            Wisconsin 2015 Other.Public     65000
    ## 1420            Wisconsin 2015    Uninsured    387400
    ## 1421            Wisconsin 2015        Total   5738100
    ## 1422            Wisconsin 2016     Employer   3245000
    ## 1423            Wisconsin 2016    Non.Group    280600
    ## 1424            Wisconsin 2016     Medicaid    910100
    ## 1425            Wisconsin 2016     Medicare    805700
    ## 1426            Wisconsin 2016 Other.Public    106400
    ## 1427            Wisconsin 2016    Uninsured    415000
    ## 1428            Wisconsin 2016        Total   5766100
    ## 1429              Wyoming 2013    Uninsured     91300
    ## 1430              Wyoming 2013 Other.Public     25800
    ## 1431              Wyoming 2013     Medicare     65400
    ## 1432              Wyoming 2013     Employer    305900
    ## 1433              Wyoming 2013     Medicaid     74200
    ## 1434              Wyoming 2013    Non.Group     19500
    ## 1435              Wyoming 2013        Total    582200
    ## 1436              Wyoming 2014    Uninsured     58100
    ## 1437              Wyoming 2014 Other.Public     13600
    ## 1438              Wyoming 2014     Medicare     65600
    ## 1439              Wyoming 2014     Employer    348200
    ## 1440              Wyoming 2014     Medicaid     54900
    ## 1441              Wyoming 2014    Non.Group     31600
    ## 1442              Wyoming 2014        Total    572000
    ## 1443              Wyoming 2015     Employer    320200
    ## 1444              Wyoming 2015    Non.Group     39300
    ## 1445              Wyoming 2015     Medicaid     59900
    ## 1446              Wyoming 2015     Medicare     78100
    ## 1447              Wyoming 2015 Other.Public     23900
    ## 1448              Wyoming 2015    Uninsured     53400
    ## 1449              Wyoming 2015        Total    574800
    ## 1450              Wyoming 2016     Employer    312200
    ## 1451              Wyoming 2016    Non.Group     37400
    ## 1452              Wyoming 2016     Medicaid     72100
    ## 1453              Wyoming 2016     Medicare     78000
    ## 1454              Wyoming 2016 Other.Public     16400
    ## 1455              Wyoming 2016    Uninsured     55500
    ## 1456              Wyoming 2016        Total    571700
    ##                 Category.y Total.Amount
    ## 1    Total.Health.Spending        33788
    ## 2    Total.Health.Spending        33788
    ## 3    Total.Health.Spending        33788
    ## 4    Total.Health.Spending        33788
    ## 5    Total.Health.Spending        33788
    ## 6    Total.Health.Spending        33788
    ## 7    Total.Health.Spending        33788
    ## 8    Total.Health.Spending        35263
    ## 9    Total.Health.Spending        35263
    ## 10   Total.Health.Spending        35263
    ## 11   Total.Health.Spending        35263
    ## 12   Total.Health.Spending        35263
    ## 13   Total.Health.Spending        35263
    ## 14   Total.Health.Spending        35263
    ## 15                    <NA>           NA
    ## 16                    <NA>           NA
    ## 17                    <NA>           NA
    ## 18                    <NA>           NA
    ## 19                    <NA>           NA
    ## 20                    <NA>           NA
    ## 21                    <NA>           NA
    ## 22                    <NA>           NA
    ## 23                    <NA>           NA
    ## 24                    <NA>           NA
    ## 25                    <NA>           NA
    ## 26                    <NA>           NA
    ## 27                    <NA>           NA
    ## 28                    <NA>           NA
    ## 29   Total.Health.Spending         7684
    ## 30   Total.Health.Spending         7684
    ## 31   Total.Health.Spending         7684
    ## 32   Total.Health.Spending         7684
    ## 33   Total.Health.Spending         7684
    ## 34   Total.Health.Spending         7684
    ## 35   Total.Health.Spending         7684
    ## 36   Total.Health.Spending         8151
    ## 37   Total.Health.Spending         8151
    ## 38   Total.Health.Spending         8151
    ## 39   Total.Health.Spending         8151
    ## 40   Total.Health.Spending         8151
    ## 41   Total.Health.Spending         8151
    ## 42   Total.Health.Spending         8151
    ## 43                    <NA>           NA
    ## 44                    <NA>           NA
    ## 45                    <NA>           NA
    ## 46                    <NA>           NA
    ## 47                    <NA>           NA
    ## 48                    <NA>           NA
    ## 49                    <NA>           NA
    ## 50                    <NA>           NA
    ## 51                    <NA>           NA
    ## 52                    <NA>           NA
    ## 53                    <NA>           NA
    ## 54                    <NA>           NA
    ## 55                    <NA>           NA
    ## 56                    <NA>           NA
    ## 57   Total.Health.Spending        41481
    ## 58   Total.Health.Spending        41481
    ## 59   Total.Health.Spending        41481
    ## 60   Total.Health.Spending        41481
    ## 61   Total.Health.Spending        41481
    ## 62   Total.Health.Spending        41481
    ## 63   Total.Health.Spending        41481
    ## 64   Total.Health.Spending        43356
    ## 65   Total.Health.Spending        43356
    ## 66   Total.Health.Spending        43356
    ## 67   Total.Health.Spending        43356
    ## 68   Total.Health.Spending        43356
    ## 69   Total.Health.Spending        43356
    ## 70   Total.Health.Spending        43356
    ## 71                    <NA>           NA
    ## 72                    <NA>           NA
    ## 73                    <NA>           NA
    ## 74                    <NA>           NA
    ## 75                    <NA>           NA
    ## 76                    <NA>           NA
    ## 77                    <NA>           NA
    ## 78                    <NA>           NA
    ## 79                    <NA>           NA
    ## 80                    <NA>           NA
    ## 81                    <NA>           NA
    ## 82                    <NA>           NA
    ## 83                    <NA>           NA
    ## 84                    <NA>           NA
    ## 85   Total.Health.Spending        20500
    ## 86   Total.Health.Spending        20500
    ## 87   Total.Health.Spending        20500
    ## 88   Total.Health.Spending        20500
    ## 89   Total.Health.Spending        20500
    ## 90   Total.Health.Spending        20500
    ## 91   Total.Health.Spending        20500
    ## 92   Total.Health.Spending        21980
    ## 93   Total.Health.Spending        21980
    ## 94   Total.Health.Spending        21980
    ## 95   Total.Health.Spending        21980
    ## 96   Total.Health.Spending        21980
    ## 97   Total.Health.Spending        21980
    ## 98   Total.Health.Spending        21980
    ## 99                    <NA>           NA
    ## 100                   <NA>           NA
    ## 101                   <NA>           NA
    ## 102                   <NA>           NA
    ## 103                   <NA>           NA
    ## 104                   <NA>           NA
    ## 105                   <NA>           NA
    ## 106                   <NA>           NA
    ## 107                   <NA>           NA
    ## 108                   <NA>           NA
    ## 109                   <NA>           NA
    ## 110                   <NA>           NA
    ## 111                   <NA>           NA
    ## 112                   <NA>           NA
    ## 113  Total.Health.Spending       278168
    ## 114  Total.Health.Spending       278168
    ## 115  Total.Health.Spending       278168
    ## 116  Total.Health.Spending       278168
    ## 117  Total.Health.Spending       278168
    ## 118  Total.Health.Spending       278168
    ## 119  Total.Health.Spending       278168
    ## 120  Total.Health.Spending       291989
    ## 121  Total.Health.Spending       291989
    ## 122  Total.Health.Spending       291989
    ## 123  Total.Health.Spending       291989
    ## 124  Total.Health.Spending       291989
    ## 125  Total.Health.Spending       291989
    ## 126  Total.Health.Spending       291989
    ## 127                   <NA>           NA
    ## 128                   <NA>           NA
    ## 129                   <NA>           NA
    ## 130                   <NA>           NA
    ## 131                   <NA>           NA
    ## 132                   <NA>           NA
    ## 133                   <NA>           NA
    ## 134                   <NA>           NA
    ## 135                   <NA>           NA
    ## 136                   <NA>           NA
    ## 137                   <NA>           NA
    ## 138                   <NA>           NA
    ## 139                   <NA>           NA
    ## 140                   <NA>           NA
    ## 141  Total.Health.Spending        34090
    ## 142  Total.Health.Spending        34090
    ## 143  Total.Health.Spending        34090
    ## 144  Total.Health.Spending        34090
    ## 145  Total.Health.Spending        34090
    ## 146  Total.Health.Spending        34090
    ## 147  Total.Health.Spending        34090
    ## 148  Total.Health.Spending        36398
    ## 149  Total.Health.Spending        36398
    ## 150  Total.Health.Spending        36398
    ## 151  Total.Health.Spending        36398
    ## 152  Total.Health.Spending        36398
    ## 153  Total.Health.Spending        36398
    ## 154  Total.Health.Spending        36398
    ## 155                   <NA>           NA
    ## 156                   <NA>           NA
    ## 157                   <NA>           NA
    ## 158                   <NA>           NA
    ## 159                   <NA>           NA
    ## 160                   <NA>           NA
    ## 161                   <NA>           NA
    ## 162                   <NA>           NA
    ## 163                   <NA>           NA
    ## 164                   <NA>           NA
    ## 165                   <NA>           NA
    ## 166                   <NA>           NA
    ## 167                   <NA>           NA
    ## 168                   <NA>           NA
    ## 169  Total.Health.Spending        34223
    ## 170  Total.Health.Spending        34223
    ## 171  Total.Health.Spending        34223
    ## 172  Total.Health.Spending        34223
    ## 173  Total.Health.Spending        34223
    ## 174  Total.Health.Spending        34223
    ## 175  Total.Health.Spending        34223
    ## 176  Total.Health.Spending        35413
    ## 177  Total.Health.Spending        35413
    ## 178  Total.Health.Spending        35413
    ## 179  Total.Health.Spending        35413
    ## 180  Total.Health.Spending        35413
    ## 181  Total.Health.Spending        35413
    ## 182  Total.Health.Spending        35413
    ## 183                   <NA>           NA
    ## 184                   <NA>           NA
    ## 185                   <NA>           NA
    ## 186                   <NA>           NA
    ## 187                   <NA>           NA
    ## 188                   <NA>           NA
    ## 189                   <NA>           NA
    ## 190                   <NA>           NA
    ## 191                   <NA>           NA
    ## 192                   <NA>           NA
    ## 193                   <NA>           NA
    ## 194                   <NA>           NA
    ## 195                   <NA>           NA
    ## 196                   <NA>           NA
    ## 197  Total.Health.Spending         9038
    ## 198  Total.Health.Spending         9038
    ## 199  Total.Health.Spending         9038
    ## 200  Total.Health.Spending         9038
    ## 201  Total.Health.Spending         9038
    ## 202  Total.Health.Spending         9038
    ## 203  Total.Health.Spending         9038
    ## 204  Total.Health.Spending         9587
    ## 205  Total.Health.Spending         9587
    ## 206  Total.Health.Spending         9587
    ## 207  Total.Health.Spending         9587
    ## 208  Total.Health.Spending         9587
    ## 209  Total.Health.Spending         9587
    ## 210  Total.Health.Spending         9587
    ## 211                   <NA>           NA
    ## 212                   <NA>           NA
    ## 213                   <NA>           NA
    ## 214                   <NA>           NA
    ## 215                   <NA>           NA
    ## 216                   <NA>           NA
    ## 217                   <NA>           NA
    ## 218                   <NA>           NA
    ## 219                   <NA>           NA
    ## 220                   <NA>           NA
    ## 221                   <NA>           NA
    ## 222                   <NA>           NA
    ## 223                   <NA>           NA
    ## 224                   <NA>           NA
    ## 225  Total.Health.Spending         7443
    ## 226  Total.Health.Spending         7443
    ## 227  Total.Health.Spending         7443
    ## 228  Total.Health.Spending         7443
    ## 229  Total.Health.Spending         7443
    ## 230  Total.Health.Spending         7443
    ## 231  Total.Health.Spending         7443
    ## 232  Total.Health.Spending         7871
    ## 233  Total.Health.Spending         7871
    ## 234  Total.Health.Spending         7871
    ## 235  Total.Health.Spending         7871
    ## 236  Total.Health.Spending         7871
    ## 237  Total.Health.Spending         7871
    ## 238  Total.Health.Spending         7871
    ## 239                   <NA>           NA
    ## 240                   <NA>           NA
    ## 241                   <NA>           NA
    ## 242                   <NA>           NA
    ## 243                   <NA>           NA
    ## 244                   <NA>           NA
    ## 245                   <NA>           NA
    ## 246                   <NA>           NA
    ## 247                   <NA>           NA
    ## 248                   <NA>           NA
    ## 249                   <NA>           NA
    ## 250                   <NA>           NA
    ## 251                   <NA>           NA
    ## 252                   <NA>           NA
    ## 253  Total.Health.Spending       150547
    ## 254  Total.Health.Spending       150547
    ## 255  Total.Health.Spending       150547
    ## 256  Total.Health.Spending       150547
    ## 257  Total.Health.Spending       150547
    ## 258  Total.Health.Spending       150547
    ## 259  Total.Health.Spending       150547
    ## 260  Total.Health.Spending       160624
    ## 261  Total.Health.Spending       160624
    ## 262  Total.Health.Spending       160624
    ## 263  Total.Health.Spending       160624
    ## 264  Total.Health.Spending       160624
    ## 265  Total.Health.Spending       160624
    ## 266  Total.Health.Spending       160624
    ## 267                   <NA>           NA
    ## 268                   <NA>           NA
    ## 269                   <NA>           NA
    ## 270                   <NA>           NA
    ## 271                   <NA>           NA
    ## 272                   <NA>           NA
    ## 273                   <NA>           NA
    ## 274                   <NA>           NA
    ## 275                   <NA>           NA
    ## 276                   <NA>           NA
    ## 277                   <NA>           NA
    ## 278                   <NA>           NA
    ## 279                   <NA>           NA
    ## 280                   <NA>           NA
    ## 281  Total.Health.Spending        62399
    ## 282  Total.Health.Spending        62399
    ## 283  Total.Health.Spending        62399
    ## 284  Total.Health.Spending        62399
    ## 285  Total.Health.Spending        62399
    ## 286  Total.Health.Spending        62399
    ## 287  Total.Health.Spending        62399
    ## 288  Total.Health.Spending        66447
    ## 289  Total.Health.Spending        66447
    ## 290  Total.Health.Spending        66447
    ## 291  Total.Health.Spending        66447
    ## 292  Total.Health.Spending        66447
    ## 293  Total.Health.Spending        66447
    ## 294  Total.Health.Spending        66447
    ## 295                   <NA>           NA
    ## 296                   <NA>           NA
    ## 297                   <NA>           NA
    ## 298                   <NA>           NA
    ## 299                   <NA>           NA
    ## 300                   <NA>           NA
    ## 301                   <NA>           NA
    ## 302                   <NA>           NA
    ## 303                   <NA>           NA
    ## 304                   <NA>           NA
    ## 305                   <NA>           NA
    ## 306                   <NA>           NA
    ## 307                   <NA>           NA
    ## 308                   <NA>           NA
    ## 309  Total.Health.Spending         9781
    ## 310  Total.Health.Spending         9781
    ## 311  Total.Health.Spending         9781
    ## 312  Total.Health.Spending         9781
    ## 313  Total.Health.Spending         9781
    ## 314  Total.Health.Spending         9781
    ## 315  Total.Health.Spending         9781
    ## 316  Total.Health.Spending        10338
    ## 317  Total.Health.Spending        10338
    ## 318  Total.Health.Spending        10338
    ## 319  Total.Health.Spending        10338
    ## 320  Total.Health.Spending        10338
    ## 321  Total.Health.Spending        10338
    ## 322  Total.Health.Spending        10338
    ## 323                   <NA>           NA
    ## 324                   <NA>           NA
    ## 325                   <NA>           NA
    ## 326                   <NA>           NA
    ## 327                   <NA>           NA
    ## 328                   <NA>           NA
    ## 329                   <NA>           NA
    ## 330                   <NA>           NA
    ## 331                   <NA>           NA
    ## 332                   <NA>           NA
    ## 333                   <NA>           NA
    ## 334                   <NA>           NA
    ## 335                   <NA>           NA
    ## 336                   <NA>           NA
    ## 337  Total.Health.Spending        10627
    ## 338  Total.Health.Spending        10627
    ## 339  Total.Health.Spending        10627
    ## 340  Total.Health.Spending        10627
    ## 341  Total.Health.Spending        10627
    ## 342  Total.Health.Spending        10627
    ## 343  Total.Health.Spending        10627
    ## 344  Total.Health.Spending        11315
    ## 345  Total.Health.Spending        11315
    ## 346  Total.Health.Spending        11315
    ## 347  Total.Health.Spending        11315
    ## 348  Total.Health.Spending        11315
    ## 349  Total.Health.Spending        11315
    ## 350  Total.Health.Spending        11315
    ## 351                   <NA>           NA
    ## 352                   <NA>           NA
    ## 353                   <NA>           NA
    ## 354                   <NA>           NA
    ## 355                   <NA>           NA
    ## 356                   <NA>           NA
    ## 357                   <NA>           NA
    ## 358                   <NA>           NA
    ## 359                   <NA>           NA
    ## 360                   <NA>           NA
    ## 361                   <NA>           NA
    ## 362                   <NA>           NA
    ## 363                   <NA>           NA
    ## 364                   <NA>           NA
    ## 365  Total.Health.Spending       101891
    ## 366  Total.Health.Spending       101891
    ## 367  Total.Health.Spending       101891
    ## 368  Total.Health.Spending       101891
    ## 369  Total.Health.Spending       101891
    ## 370  Total.Health.Spending       101891
    ## 371  Total.Health.Spending       101891
    ## 372  Total.Health.Spending       106306
    ## 373  Total.Health.Spending       106306
    ## 374  Total.Health.Spending       106306
    ## 375  Total.Health.Spending       106306
    ## 376  Total.Health.Spending       106306
    ## 377  Total.Health.Spending       106306
    ## 378  Total.Health.Spending       106306
    ## 379                   <NA>           NA
    ## 380                   <NA>           NA
    ## 381                   <NA>           NA
    ## 382                   <NA>           NA
    ## 383                   <NA>           NA
    ## 384                   <NA>           NA
    ## 385                   <NA>           NA
    ## 386                   <NA>           NA
    ## 387                   <NA>           NA
    ## 388                   <NA>           NA
    ## 389                   <NA>           NA
    ## 390                   <NA>           NA
    ## 391                   <NA>           NA
    ## 392                   <NA>           NA
    ## 393  Total.Health.Spending        52046
    ## 394  Total.Health.Spending        52046
    ## 395  Total.Health.Spending        52046
    ## 396  Total.Health.Spending        52046
    ## 397  Total.Health.Spending        52046
    ## 398  Total.Health.Spending        52046
    ## 399  Total.Health.Spending        52046
    ## 400  Total.Health.Spending        54741
    ## 401  Total.Health.Spending        54741
    ## 402  Total.Health.Spending        54741
    ## 403  Total.Health.Spending        54741
    ## 404  Total.Health.Spending        54741
    ## 405  Total.Health.Spending        54741
    ## 406  Total.Health.Spending        54741
    ## 407                   <NA>           NA
    ## 408                   <NA>           NA
    ## 409                   <NA>           NA
    ## 410                   <NA>           NA
    ## 411                   <NA>           NA
    ## 412                   <NA>           NA
    ## 413                   <NA>           NA
    ## 414                   <NA>           NA
    ## 415                   <NA>           NA
    ## 416                   <NA>           NA
    ## 417                   <NA>           NA
    ## 418                   <NA>           NA
    ## 419                   <NA>           NA
    ## 420                   <NA>           NA
    ## 421  Total.Health.Spending        24135
    ## 422  Total.Health.Spending        24135
    ## 423  Total.Health.Spending        24135
    ## 424  Total.Health.Spending        24135
    ## 425  Total.Health.Spending        24135
    ## 426  Total.Health.Spending        24135
    ## 427  Total.Health.Spending        24135
    ## 428  Total.Health.Spending        25487
    ## 429  Total.Health.Spending        25487
    ## 430  Total.Health.Spending        25487
    ## 431  Total.Health.Spending        25487
    ## 432  Total.Health.Spending        25487
    ## 433  Total.Health.Spending        25487
    ## 434  Total.Health.Spending        25487
    ## 435                   <NA>           NA
    ## 436                   <NA>           NA
    ## 437                   <NA>           NA
    ## 438                   <NA>           NA
    ## 439                   <NA>           NA
    ## 440                   <NA>           NA
    ## 441                   <NA>           NA
    ## 442                   <NA>           NA
    ## 443                   <NA>           NA
    ## 444                   <NA>           NA
    ## 445                   <NA>           NA
    ## 446                   <NA>           NA
    ## 447                   <NA>           NA
    ## 448                   <NA>           NA
    ## 449  Total.Health.Spending        21490
    ## 450  Total.Health.Spending        21490
    ## 451  Total.Health.Spending        21490
    ## 452  Total.Health.Spending        21490
    ## 453  Total.Health.Spending        21490
    ## 454  Total.Health.Spending        21490
    ## 455  Total.Health.Spending        21490
    ## 456  Total.Health.Spending        22183
    ## 457  Total.Health.Spending        22183
    ## 458  Total.Health.Spending        22183
    ## 459  Total.Health.Spending        22183
    ## 460  Total.Health.Spending        22183
    ## 461  Total.Health.Spending        22183
    ## 462  Total.Health.Spending        22183
    ## 463                   <NA>           NA
    ## 464                   <NA>           NA
    ## 465                   <NA>           NA
    ## 466                   <NA>           NA
    ## 467                   <NA>           NA
    ## 468                   <NA>           NA
    ## 469                   <NA>           NA
    ## 470                   <NA>           NA
    ## 471                   <NA>           NA
    ## 472                   <NA>           NA
    ## 473                   <NA>           NA
    ## 474                   <NA>           NA
    ## 475                   <NA>           NA
    ## 476                   <NA>           NA
    ## 477  Total.Health.Spending        33194
    ## 478  Total.Health.Spending        33194
    ## 479  Total.Health.Spending        33194
    ## 480  Total.Health.Spending        33194
    ## 481  Total.Health.Spending        33194
    ## 482  Total.Health.Spending        33194
    ## 483  Total.Health.Spending        33194
    ## 484  Total.Health.Spending        35323
    ## 485  Total.Health.Spending        35323
    ## 486  Total.Health.Spending        35323
    ## 487  Total.Health.Spending        35323
    ## 488  Total.Health.Spending        35323
    ## 489  Total.Health.Spending        35323
    ## 490  Total.Health.Spending        35323
    ## 491                   <NA>           NA
    ## 492                   <NA>           NA
    ## 493                   <NA>           NA
    ## 494                   <NA>           NA
    ## 495                   <NA>           NA
    ## 496                   <NA>           NA
    ## 497                   <NA>           NA
    ## 498                   <NA>           NA
    ## 499                   <NA>           NA
    ## 500                   <NA>           NA
    ## 501                   <NA>           NA
    ## 502                   <NA>           NA
    ## 503                   <NA>           NA
    ## 504                   <NA>           NA
    ## 505  Total.Health.Spending        34639
    ## 506  Total.Health.Spending        34639
    ## 507  Total.Health.Spending        34639
    ## 508  Total.Health.Spending        34639
    ## 509  Total.Health.Spending        34639
    ## 510  Total.Health.Spending        34639
    ## 511  Total.Health.Spending        34639
    ## 512  Total.Health.Spending        36324
    ## 513  Total.Health.Spending        36324
    ## 514  Total.Health.Spending        36324
    ## 515  Total.Health.Spending        36324
    ## 516  Total.Health.Spending        36324
    ## 517  Total.Health.Spending        36324
    ## 518  Total.Health.Spending        36324
    ## 519                   <NA>           NA
    ## 520                   <NA>           NA
    ## 521                   <NA>           NA
    ## 522                   <NA>           NA
    ## 523                   <NA>           NA
    ## 524                   <NA>           NA
    ## 525                   <NA>           NA
    ## 526                   <NA>           NA
    ## 527                   <NA>           NA
    ## 528                   <NA>           NA
    ## 529                   <NA>           NA
    ## 530                   <NA>           NA
    ## 531                   <NA>           NA
    ## 532                   <NA>           NA
    ## 533  Total.Health.Spending        12139
    ## 534  Total.Health.Spending        12139
    ## 535  Total.Health.Spending        12139
    ## 536  Total.Health.Spending        12139
    ## 537  Total.Health.Spending        12139
    ## 538  Total.Health.Spending        12139
    ## 539  Total.Health.Spending        12139
    ## 540  Total.Health.Spending        12684
    ## 541  Total.Health.Spending        12684
    ## 542  Total.Health.Spending        12684
    ## 543  Total.Health.Spending        12684
    ## 544  Total.Health.Spending        12684
    ## 545  Total.Health.Spending        12684
    ## 546  Total.Health.Spending        12684
    ## 547                   <NA>           NA
    ## 548                   <NA>           NA
    ## 549                   <NA>           NA
    ## 550                   <NA>           NA
    ## 551                   <NA>           NA
    ## 552                   <NA>           NA
    ## 553                   <NA>           NA
    ## 554                   <NA>           NA
    ## 555                   <NA>           NA
    ## 556                   <NA>           NA
    ## 557                   <NA>           NA
    ## 558                   <NA>           NA
    ## 559                   <NA>           NA
    ## 560                   <NA>           NA
    ## 561  Total.Health.Spending        48929
    ## 562  Total.Health.Spending        48929
    ## 563  Total.Health.Spending        48929
    ## 564  Total.Health.Spending        48929
    ## 565  Total.Health.Spending        48929
    ## 566  Total.Health.Spending        48929
    ## 567  Total.Health.Spending        48929
    ## 568  Total.Health.Spending        51330
    ## 569  Total.Health.Spending        51330
    ## 570  Total.Health.Spending        51330
    ## 571  Total.Health.Spending        51330
    ## 572  Total.Health.Spending        51330
    ## 573  Total.Health.Spending        51330
    ## 574  Total.Health.Spending        51330
    ## 575                   <NA>           NA
    ## 576                   <NA>           NA
    ## 577                   <NA>           NA
    ## 578                   <NA>           NA
    ## 579                   <NA>           NA
    ## 580                   <NA>           NA
    ## 581                   <NA>           NA
    ## 582                   <NA>           NA
    ## 583                   <NA>           NA
    ## 584                   <NA>           NA
    ## 585                   <NA>           NA
    ## 586                   <NA>           NA
    ## 587                   <NA>           NA
    ## 588                   <NA>           NA
    ## 589  Total.Health.Spending        68899
    ## 590  Total.Health.Spending        68899
    ## 591  Total.Health.Spending        68899
    ## 592  Total.Health.Spending        68899
    ## 593  Total.Health.Spending        68899
    ## 594  Total.Health.Spending        68899
    ## 595  Total.Health.Spending        68899
    ## 596  Total.Health.Spending        71274
    ## 597  Total.Health.Spending        71274
    ## 598  Total.Health.Spending        71274
    ## 599  Total.Health.Spending        71274
    ## 600  Total.Health.Spending        71274
    ## 601  Total.Health.Spending        71274
    ## 602  Total.Health.Spending        71274
    ## 603                   <NA>           NA
    ## 604                   <NA>           NA
    ## 605                   <NA>           NA
    ## 606                   <NA>           NA
    ## 607                   <NA>           NA
    ## 608                   <NA>           NA
    ## 609                   <NA>           NA
    ## 610                   <NA>           NA
    ## 611                   <NA>           NA
    ## 612                   <NA>           NA
    ## 613                   <NA>           NA
    ## 614                   <NA>           NA
    ## 615                   <NA>           NA
    ## 616                   <NA>           NA
    ## 617  Total.Health.Spending        76672
    ## 618  Total.Health.Spending        76672
    ## 619  Total.Health.Spending        76672
    ## 620  Total.Health.Spending        76672
    ## 621  Total.Health.Spending        76672
    ## 622  Total.Health.Spending        76672
    ## 623  Total.Health.Spending        76672
    ## 624  Total.Health.Spending        79874
    ## 625  Total.Health.Spending        79874
    ## 626  Total.Health.Spending        79874
    ## 627  Total.Health.Spending        79874
    ## 628  Total.Health.Spending        79874
    ## 629  Total.Health.Spending        79874
    ## 630  Total.Health.Spending        79874
    ## 631                   <NA>           NA
    ## 632                   <NA>           NA
    ## 633                   <NA>           NA
    ## 634                   <NA>           NA
    ## 635                   <NA>           NA
    ## 636                   <NA>           NA
    ## 637                   <NA>           NA
    ## 638                   <NA>           NA
    ## 639                   <NA>           NA
    ## 640                   <NA>           NA
    ## 641                   <NA>           NA
    ## 642                   <NA>           NA
    ## 643                   <NA>           NA
    ## 644                   <NA>           NA
    ## 645  Total.Health.Spending        45865
    ## 646  Total.Health.Spending        45865
    ## 647  Total.Health.Spending        45865
    ## 648  Total.Health.Spending        45865
    ## 649  Total.Health.Spending        45865
    ## 650  Total.Health.Spending        45865
    ## 651  Total.Health.Spending        45865
    ## 652  Total.Health.Spending        48377
    ## 653  Total.Health.Spending        48377
    ## 654  Total.Health.Spending        48377
    ## 655  Total.Health.Spending        48377
    ## 656  Total.Health.Spending        48377
    ## 657  Total.Health.Spending        48377
    ## 658  Total.Health.Spending        48377
    ## 659                   <NA>           NA
    ## 660                   <NA>           NA
    ## 661                   <NA>           NA
    ## 662                   <NA>           NA
    ## 663                   <NA>           NA
    ## 664                   <NA>           NA
    ## 665                   <NA>           NA
    ## 666                   <NA>           NA
    ## 667                   <NA>           NA
    ## 668                   <NA>           NA
    ## 669                   <NA>           NA
    ## 670                   <NA>           NA
    ## 671                   <NA>           NA
    ## 672                   <NA>           NA
    ## 673  Total.Health.Spending        22017
    ## 674  Total.Health.Spending        22017
    ## 675  Total.Health.Spending        22017
    ## 676  Total.Health.Spending        22017
    ## 677  Total.Health.Spending        22017
    ## 678  Total.Health.Spending        22017
    ## 679  Total.Health.Spending        22017
    ## 680  Total.Health.Spending        22879
    ## 681  Total.Health.Spending        22879
    ## 682  Total.Health.Spending        22879
    ## 683  Total.Health.Spending        22879
    ## 684  Total.Health.Spending        22879
    ## 685  Total.Health.Spending        22879
    ## 686  Total.Health.Spending        22879
    ## 687                   <NA>           NA
    ## 688                   <NA>           NA
    ## 689                   <NA>           NA
    ## 690                   <NA>           NA
    ## 691                   <NA>           NA
    ## 692                   <NA>           NA
    ## 693                   <NA>           NA
    ## 694                   <NA>           NA
    ## 695                   <NA>           NA
    ## 696                   <NA>           NA
    ## 697                   <NA>           NA
    ## 698                   <NA>           NA
    ## 699                   <NA>           NA
    ## 700                   <NA>           NA
    ## 701  Total.Health.Spending        47499
    ## 702  Total.Health.Spending        47499
    ## 703  Total.Health.Spending        47499
    ## 704  Total.Health.Spending        47499
    ## 705  Total.Health.Spending        47499
    ## 706  Total.Health.Spending        47499
    ## 707  Total.Health.Spending        47499
    ## 708  Total.Health.Spending        49137
    ## 709  Total.Health.Spending        49137
    ## 710  Total.Health.Spending        49137
    ## 711  Total.Health.Spending        49137
    ## 712  Total.Health.Spending        49137
    ## 713  Total.Health.Spending        49137
    ## 714  Total.Health.Spending        49137
    ## 715                   <NA>           NA
    ## 716                   <NA>           NA
    ## 717                   <NA>           NA
    ## 718                   <NA>           NA
    ## 719                   <NA>           NA
    ## 720                   <NA>           NA
    ## 721                   <NA>           NA
    ## 722                   <NA>           NA
    ## 723                   <NA>           NA
    ## 724                   <NA>           NA
    ## 725                   <NA>           NA
    ## 726                   <NA>           NA
    ## 727                   <NA>           NA
    ## 728                   <NA>           NA
    ## 729  Total.Health.Spending         8108
    ## 730  Total.Health.Spending         8108
    ## 731  Total.Health.Spending         8108
    ## 732  Total.Health.Spending         8108
    ## 733  Total.Health.Spending         8108
    ## 734  Total.Health.Spending         8108
    ## 735  Total.Health.Spending         8108
    ## 736  Total.Health.Spending         8409
    ## 737  Total.Health.Spending         8409
    ## 738  Total.Health.Spending         8409
    ## 739  Total.Health.Spending         8409
    ## 740  Total.Health.Spending         8409
    ## 741  Total.Health.Spending         8409
    ## 742  Total.Health.Spending         8409
    ## 743                   <NA>           NA
    ## 744                   <NA>           NA
    ## 745                   <NA>           NA
    ## 746                   <NA>           NA
    ## 747                   <NA>           NA
    ## 748                   <NA>           NA
    ## 749                   <NA>           NA
    ## 750                   <NA>           NA
    ## 751                   <NA>           NA
    ## 752                   <NA>           NA
    ## 753                   <NA>           NA
    ## 754                   <NA>           NA
    ## 755                   <NA>           NA
    ## 756                   <NA>           NA
    ## 757  Total.Health.Spending        15197
    ## 758  Total.Health.Spending        15197
    ## 759  Total.Health.Spending        15197
    ## 760  Total.Health.Spending        15197
    ## 761  Total.Health.Spending        15197
    ## 762  Total.Health.Spending        15197
    ## 763  Total.Health.Spending        15197
    ## 764  Total.Health.Spending        15823
    ## 765  Total.Health.Spending        15823
    ## 766  Total.Health.Spending        15823
    ## 767  Total.Health.Spending        15823
    ## 768  Total.Health.Spending        15823
    ## 769  Total.Health.Spending        15823
    ## 770  Total.Health.Spending        15823
    ## 771                   <NA>           NA
    ## 772                   <NA>           NA
    ## 773                   <NA>           NA
    ## 774                   <NA>           NA
    ## 775                   <NA>           NA
    ## 776                   <NA>           NA
    ## 777                   <NA>           NA
    ## 778                   <NA>           NA
    ## 779                   <NA>           NA
    ## 780                   <NA>           NA
    ## 781                   <NA>           NA
    ## 782                   <NA>           NA
    ## 783                   <NA>           NA
    ## 784                   <NA>           NA
    ## 785  Total.Health.Spending        17485
    ## 786  Total.Health.Spending        17485
    ## 787  Total.Health.Spending        17485
    ## 788  Total.Health.Spending        17485
    ## 789  Total.Health.Spending        17485
    ## 790  Total.Health.Spending        17485
    ## 791  Total.Health.Spending        17485
    ## 792  Total.Health.Spending        19020
    ## 793  Total.Health.Spending        19020
    ## 794  Total.Health.Spending        19020
    ## 795  Total.Health.Spending        19020
    ## 796  Total.Health.Spending        19020
    ## 797  Total.Health.Spending        19020
    ## 798  Total.Health.Spending        19020
    ## 799                   <NA>           NA
    ## 800                   <NA>           NA
    ## 801                   <NA>           NA
    ## 802                   <NA>           NA
    ## 803                   <NA>           NA
    ## 804                   <NA>           NA
    ## 805                   <NA>           NA
    ## 806                   <NA>           NA
    ## 807                   <NA>           NA
    ## 808                   <NA>           NA
    ## 809                   <NA>           NA
    ## 810                   <NA>           NA
    ## 811                   <NA>           NA
    ## 812                   <NA>           NA
    ## 813  Total.Health.Spending        12392
    ## 814  Total.Health.Spending        12392
    ## 815  Total.Health.Spending        12392
    ## 816  Total.Health.Spending        12392
    ## 817  Total.Health.Spending        12392
    ## 818  Total.Health.Spending        12392
    ## 819  Total.Health.Spending        12392
    ## 820  Total.Health.Spending        12742
    ## 821  Total.Health.Spending        12742
    ## 822  Total.Health.Spending        12742
    ## 823  Total.Health.Spending        12742
    ## 824  Total.Health.Spending        12742
    ## 825  Total.Health.Spending        12742
    ## 826  Total.Health.Spending        12742
    ## 827                   <NA>           NA
    ## 828                   <NA>           NA
    ## 829                   <NA>           NA
    ## 830                   <NA>           NA
    ## 831                   <NA>           NA
    ## 832                   <NA>           NA
    ## 833                   <NA>           NA
    ## 834                   <NA>           NA
    ## 835                   <NA>           NA
    ## 836                   <NA>           NA
    ## 837                   <NA>           NA
    ## 838                   <NA>           NA
    ## 839                   <NA>           NA
    ## 840                   <NA>           NA
    ## 841  Total.Health.Spending        75148
    ## 842  Total.Health.Spending        75148
    ## 843  Total.Health.Spending        75148
    ## 844  Total.Health.Spending        75148
    ## 845  Total.Health.Spending        75148
    ## 846  Total.Health.Spending        75148
    ## 847  Total.Health.Spending        75148
    ## 848  Total.Health.Spending        79066
    ## 849  Total.Health.Spending        79066
    ## 850  Total.Health.Spending        79066
    ## 851  Total.Health.Spending        79066
    ## 852  Total.Health.Spending        79066
    ## 853  Total.Health.Spending        79066
    ## 854  Total.Health.Spending        79066
    ## 855                   <NA>           NA
    ## 856                   <NA>           NA
    ## 857                   <NA>           NA
    ## 858                   <NA>           NA
    ## 859                   <NA>           NA
    ## 860                   <NA>           NA
    ## 861                   <NA>           NA
    ## 862                   <NA>           NA
    ## 863                   <NA>           NA
    ## 864                   <NA>           NA
    ## 865                   <NA>           NA
    ## 866                   <NA>           NA
    ## 867                   <NA>           NA
    ## 868                   <NA>           NA
    ## 869  Total.Health.Spending        14304
    ## 870  Total.Health.Spending        14304
    ## 871  Total.Health.Spending        14304
    ## 872  Total.Health.Spending        14304
    ## 873  Total.Health.Spending        14304
    ## 874  Total.Health.Spending        14304
    ## 875  Total.Health.Spending        14304
    ## 876  Total.Health.Spending        15027
    ## 877  Total.Health.Spending        15027
    ## 878  Total.Health.Spending        15027
    ## 879  Total.Health.Spending        15027
    ## 880  Total.Health.Spending        15027
    ## 881  Total.Health.Spending        15027
    ## 882  Total.Health.Spending        15027
    ## 883                   <NA>           NA
    ## 884                   <NA>           NA
    ## 885                   <NA>           NA
    ## 886                   <NA>           NA
    ## 887                   <NA>           NA
    ## 888                   <NA>           NA
    ## 889                   <NA>           NA
    ## 890                   <NA>           NA
    ## 891                   <NA>           NA
    ## 892                   <NA>           NA
    ## 893                   <NA>           NA
    ## 894                   <NA>           NA
    ## 895                   <NA>           NA
    ## 896                   <NA>           NA
    ## 897  Total.Health.Spending       183969
    ## 898  Total.Health.Spending       183969
    ## 899  Total.Health.Spending       183969
    ## 900  Total.Health.Spending       183969
    ## 901  Total.Health.Spending       183969
    ## 902  Total.Health.Spending       183969
    ## 903  Total.Health.Spending       183969
    ## 904  Total.Health.Spending       192809
    ## 905  Total.Health.Spending       192809
    ## 906  Total.Health.Spending       192809
    ## 907  Total.Health.Spending       192809
    ## 908  Total.Health.Spending       192809
    ## 909  Total.Health.Spending       192809
    ## 910  Total.Health.Spending       192809
    ## 911                   <NA>           NA
    ## 912                   <NA>           NA
    ## 913                   <NA>           NA
    ## 914                   <NA>           NA
    ## 915                   <NA>           NA
    ## 916                   <NA>           NA
    ## 917                   <NA>           NA
    ## 918                   <NA>           NA
    ## 919                   <NA>           NA
    ## 920                   <NA>           NA
    ## 921                   <NA>           NA
    ## 922                   <NA>           NA
    ## 923                   <NA>           NA
    ## 924                   <NA>           NA
    ## 925  Total.Health.Spending        69157
    ## 926  Total.Health.Spending        69157
    ## 927  Total.Health.Spending        69157
    ## 928  Total.Health.Spending        69157
    ## 929  Total.Health.Spending        69157
    ## 930  Total.Health.Spending        69157
    ## 931  Total.Health.Spending        69157
    ## 932  Total.Health.Spending        72160
    ## 933  Total.Health.Spending        72160
    ## 934  Total.Health.Spending        72160
    ## 935  Total.Health.Spending        72160
    ## 936  Total.Health.Spending        72160
    ## 937  Total.Health.Spending        72160
    ## 938  Total.Health.Spending        72160
    ## 939                   <NA>           NA
    ## 940                   <NA>           NA
    ## 941                   <NA>           NA
    ## 942                   <NA>           NA
    ## 943                   <NA>           NA
    ## 944                   <NA>           NA
    ## 945                   <NA>           NA
    ## 946                   <NA>           NA
    ## 947                   <NA>           NA
    ## 948                   <NA>           NA
    ## 949                   <NA>           NA
    ## 950                   <NA>           NA
    ## 951                   <NA>           NA
    ## 952                   <NA>           NA
    ## 953  Total.Health.Spending         6795
    ## 954  Total.Health.Spending         6795
    ## 955  Total.Health.Spending         6795
    ## 956  Total.Health.Spending         6795
    ## 957  Total.Health.Spending         6795
    ## 958  Total.Health.Spending         6795
    ## 959  Total.Health.Spending         6795
    ## 960  Total.Health.Spending         7289
    ## 961  Total.Health.Spending         7289
    ## 962  Total.Health.Spending         7289
    ## 963  Total.Health.Spending         7289
    ## 964  Total.Health.Spending         7289
    ## 965  Total.Health.Spending         7289
    ## 966  Total.Health.Spending         7289
    ## 967                   <NA>           NA
    ## 968                   <NA>           NA
    ## 969                   <NA>           NA
    ## 970                   <NA>           NA
    ## 971                   <NA>           NA
    ## 972                   <NA>           NA
    ## 973                   <NA>           NA
    ## 974                   <NA>           NA
    ## 975                   <NA>           NA
    ## 976                   <NA>           NA
    ## 977                   <NA>           NA
    ## 978                   <NA>           NA
    ## 979                   <NA>           NA
    ## 980                   <NA>           NA
    ## 981  Total.Health.Spending        95866
    ## 982  Total.Health.Spending        95866
    ## 983  Total.Health.Spending        95866
    ## 984  Total.Health.Spending        95866
    ## 985  Total.Health.Spending        95866
    ## 986  Total.Health.Spending        95866
    ## 987  Total.Health.Spending        95866
    ## 988  Total.Health.Spending       101013
    ## 989  Total.Health.Spending       101013
    ## 990  Total.Health.Spending       101013
    ## 991  Total.Health.Spending       101013
    ## 992  Total.Health.Spending       101013
    ## 993  Total.Health.Spending       101013
    ## 994  Total.Health.Spending       101013
    ## 995                   <NA>           NA
    ## 996                   <NA>           NA
    ## 997                   <NA>           NA
    ## 998                   <NA>           NA
    ## 999                   <NA>           NA
    ## 1000                  <NA>           NA
    ## 1001                  <NA>           NA
    ## 1002                  <NA>           NA
    ## 1003                  <NA>           NA
    ## 1004                  <NA>           NA
    ## 1005                  <NA>           NA
    ## 1006                  <NA>           NA
    ## 1007                  <NA>           NA
    ## 1008                  <NA>           NA
    ## 1009 Total.Health.Spending        28097
    ## 1010 Total.Health.Spending        28097
    ## 1011 Total.Health.Spending        28097
    ## 1012 Total.Health.Spending        28097
    ## 1013 Total.Health.Spending        28097
    ## 1014 Total.Health.Spending        28097
    ## 1015 Total.Health.Spending        28097
    ## 1016 Total.Health.Spending        29575
    ## 1017 Total.Health.Spending        29575
    ## 1018 Total.Health.Spending        29575
    ## 1019 Total.Health.Spending        29575
    ## 1020 Total.Health.Spending        29575
    ## 1021 Total.Health.Spending        29575
    ## 1022 Total.Health.Spending        29575
    ## 1023                  <NA>           NA
    ## 1024                  <NA>           NA
    ## 1025                  <NA>           NA
    ## 1026                  <NA>           NA
    ## 1027                  <NA>           NA
    ## 1028                  <NA>           NA
    ## 1029                  <NA>           NA
    ## 1030                  <NA>           NA
    ## 1031                  <NA>           NA
    ## 1032                  <NA>           NA
    ## 1033                  <NA>           NA
    ## 1034                  <NA>           NA
    ## 1035                  <NA>           NA
    ## 1036                  <NA>           NA
    ## 1037 Total.Health.Spending        29314
    ## 1038 Total.Health.Spending        29314
    ## 1039 Total.Health.Spending        29314
    ## 1040 Total.Health.Spending        29314
    ## 1041 Total.Health.Spending        29314
    ## 1042 Total.Health.Spending        29314
    ## 1043 Total.Health.Spending        29314
    ## 1044 Total.Health.Spending        31920
    ## 1045 Total.Health.Spending        31920
    ## 1046 Total.Health.Spending        31920
    ## 1047 Total.Health.Spending        31920
    ## 1048 Total.Health.Spending        31920
    ## 1049 Total.Health.Spending        31920
    ## 1050 Total.Health.Spending        31920
    ## 1051                  <NA>           NA
    ## 1052                  <NA>           NA
    ## 1053                  <NA>           NA
    ## 1054                  <NA>           NA
    ## 1055                  <NA>           NA
    ## 1056                  <NA>           NA
    ## 1057                  <NA>           NA
    ## 1058                  <NA>           NA
    ## 1059                  <NA>           NA
    ## 1060                  <NA>           NA
    ## 1061                  <NA>           NA
    ## 1062                  <NA>           NA
    ## 1063                  <NA>           NA
    ## 1064                  <NA>           NA
    ## 1065 Total.Health.Spending       113459
    ## 1066 Total.Health.Spending       113459
    ## 1067 Total.Health.Spending       113459
    ## 1068 Total.Health.Spending       113459
    ## 1069 Total.Health.Spending       113459
    ## 1070 Total.Health.Spending       113459
    ## 1071 Total.Health.Spending       113459
    ## 1072 Total.Health.Spending       118419
    ## 1073 Total.Health.Spending       118419
    ## 1074 Total.Health.Spending       118419
    ## 1075 Total.Health.Spending       118419
    ## 1076 Total.Health.Spending       118419
    ## 1077 Total.Health.Spending       118419
    ## 1078 Total.Health.Spending       118419
    ## 1079                  <NA>           NA
    ## 1080                  <NA>           NA
    ## 1081                  <NA>           NA
    ## 1082                  <NA>           NA
    ## 1083                  <NA>           NA
    ## 1084                  <NA>           NA
    ## 1085                  <NA>           NA
    ## 1086                  <NA>           NA
    ## 1087                  <NA>           NA
    ## 1088                  <NA>           NA
    ## 1089                  <NA>           NA
    ## 1090                  <NA>           NA
    ## 1091                  <NA>           NA
    ## 1092                  <NA>           NA
    ## 1093 Total.Health.Spending         9646
    ## 1094 Total.Health.Spending         9646
    ## 1095 Total.Health.Spending         9646
    ## 1096 Total.Health.Spending         9646
    ## 1097 Total.Health.Spending         9646
    ## 1098 Total.Health.Spending         9646
    ## 1099 Total.Health.Spending         9646
    ## 1100 Total.Health.Spending        10071
    ## 1101 Total.Health.Spending        10071
    ## 1102 Total.Health.Spending        10071
    ## 1103 Total.Health.Spending        10071
    ## 1104 Total.Health.Spending        10071
    ## 1105 Total.Health.Spending        10071
    ## 1106 Total.Health.Spending        10071
    ## 1107                  <NA>           NA
    ## 1108                  <NA>           NA
    ## 1109                  <NA>           NA
    ## 1110                  <NA>           NA
    ## 1111                  <NA>           NA
    ## 1112                  <NA>           NA
    ## 1113                  <NA>           NA
    ## 1114                  <NA>           NA
    ## 1115                  <NA>           NA
    ## 1116                  <NA>           NA
    ## 1117                  <NA>           NA
    ## 1118                  <NA>           NA
    ## 1119                  <NA>           NA
    ## 1120                  <NA>           NA
    ## 1121 Total.Health.Spending        33468
    ## 1122 Total.Health.Spending        33468
    ## 1123 Total.Health.Spending        33468
    ## 1124 Total.Health.Spending        33468
    ## 1125 Total.Health.Spending        33468
    ## 1126 Total.Health.Spending        33468
    ## 1127 Total.Health.Spending        33468
    ## 1128 Total.Health.Spending        35299
    ## 1129 Total.Health.Spending        35299
    ## 1130 Total.Health.Spending        35299
    ## 1131 Total.Health.Spending        35299
    ## 1132 Total.Health.Spending        35299
    ## 1133 Total.Health.Spending        35299
    ## 1134 Total.Health.Spending        35299
    ## 1135                  <NA>           NA
    ## 1136                  <NA>           NA
    ## 1137                  <NA>           NA
    ## 1138                  <NA>           NA
    ## 1139                  <NA>           NA
    ## 1140                  <NA>           NA
    ## 1141                  <NA>           NA
    ## 1142                  <NA>           NA
    ## 1143                  <NA>           NA
    ## 1144                  <NA>           NA
    ## 1145                  <NA>           NA
    ## 1146                  <NA>           NA
    ## 1147                  <NA>           NA
    ## 1148                  <NA>           NA
    ## 1149 Total.Health.Spending         7221
    ## 1150 Total.Health.Spending         7221
    ## 1151 Total.Health.Spending         7221
    ## 1152 Total.Health.Spending         7221
    ## 1153 Total.Health.Spending         7221
    ## 1154 Total.Health.Spending         7221
    ## 1155 Total.Health.Spending         7221
    ## 1156 Total.Health.Spending         7616
    ## 1157 Total.Health.Spending         7616
    ## 1158 Total.Health.Spending         7616
    ## 1159 Total.Health.Spending         7616
    ## 1160 Total.Health.Spending         7616
    ## 1161 Total.Health.Spending         7616
    ## 1162 Total.Health.Spending         7616
    ## 1163                  <NA>           NA
    ## 1164                  <NA>           NA
    ## 1165                  <NA>           NA
    ## 1166                  <NA>           NA
    ## 1167                  <NA>           NA
    ## 1168                  <NA>           NA
    ## 1169                  <NA>           NA
    ## 1170                  <NA>           NA
    ## 1171                  <NA>           NA
    ## 1172                  <NA>           NA
    ## 1173                  <NA>           NA
    ## 1174                  <NA>           NA
    ## 1175                  <NA>           NA
    ## 1176                  <NA>           NA
    ## 1177 Total.Health.Spending        46149
    ## 1178 Total.Health.Spending        46149
    ## 1179 Total.Health.Spending        46149
    ## 1180 Total.Health.Spending        46149
    ## 1181 Total.Health.Spending        46149
    ## 1182 Total.Health.Spending        46149
    ## 1183 Total.Health.Spending        46149
    ## 1184 Total.Health.Spending        48249
    ## 1185 Total.Health.Spending        48249
    ## 1186 Total.Health.Spending        48249
    ## 1187 Total.Health.Spending        48249
    ## 1188 Total.Health.Spending        48249
    ## 1189 Total.Health.Spending        48249
    ## 1190 Total.Health.Spending        48249
    ## 1191                  <NA>           NA
    ## 1192                  <NA>           NA
    ## 1193                  <NA>           NA
    ## 1194                  <NA>           NA
    ## 1195                  <NA>           NA
    ## 1196                  <NA>           NA
    ## 1197                  <NA>           NA
    ## 1198                  <NA>           NA
    ## 1199                  <NA>           NA
    ## 1200                  <NA>           NA
    ## 1201                  <NA>           NA
    ## 1202                  <NA>           NA
    ## 1203                  <NA>           NA
    ## 1204                  <NA>           NA
    ## 1205 Total.Health.Spending       176341
    ## 1206 Total.Health.Spending       176341
    ## 1207 Total.Health.Spending       176341
    ## 1208 Total.Health.Spending       176341
    ## 1209 Total.Health.Spending       176341
    ## 1210 Total.Health.Spending       176341
    ## 1211 Total.Health.Spending       176341
    ## 1212 Total.Health.Spending       188559
    ## 1213 Total.Health.Spending       188559
    ## 1214 Total.Health.Spending       188559
    ## 1215 Total.Health.Spending       188559
    ## 1216 Total.Health.Spending       188559
    ## 1217 Total.Health.Spending       188559
    ## 1218 Total.Health.Spending       188559
    ## 1219                  <NA>           NA
    ## 1220                  <NA>           NA
    ## 1221                  <NA>           NA
    ## 1222                  <NA>           NA
    ## 1223                  <NA>           NA
    ## 1224                  <NA>           NA
    ## 1225                  <NA>           NA
    ## 1226                  <NA>           NA
    ## 1227                  <NA>           NA
    ## 1228                  <NA>           NA
    ## 1229                  <NA>           NA
    ## 1230                  <NA>           NA
    ## 1231                  <NA>           NA
    ## 1232                  <NA>           NA
    ## 1233 Total.Health.Spending      2435624
    ## 1234 Total.Health.Spending      2435624
    ## 1235 Total.Health.Spending      2435624
    ## 1236 Total.Health.Spending      2435624
    ## 1237 Total.Health.Spending      2435624
    ## 1238 Total.Health.Spending      2435624
    ## 1239 Total.Health.Spending      2435624
    ## 1240 Total.Health.Spending      2562824
    ## 1241 Total.Health.Spending      2562824
    ## 1242 Total.Health.Spending      2562824
    ## 1243 Total.Health.Spending      2562824
    ## 1244 Total.Health.Spending      2562824
    ## 1245 Total.Health.Spending      2562824
    ## 1246 Total.Health.Spending      2562824
    ## 1247                  <NA>           NA
    ## 1248                  <NA>           NA
    ## 1249                  <NA>           NA
    ## 1250                  <NA>           NA
    ## 1251                  <NA>           NA
    ## 1252                  <NA>           NA
    ## 1253                  <NA>           NA
    ## 1254                  <NA>           NA
    ## 1255                  <NA>           NA
    ## 1256                  <NA>           NA
    ## 1257                  <NA>           NA
    ## 1258                  <NA>           NA
    ## 1259                  <NA>           NA
    ## 1260                  <NA>           NA
    ## 1261 Total.Health.Spending        16425
    ## 1262 Total.Health.Spending        16425
    ## 1263 Total.Health.Spending        16425
    ## 1264 Total.Health.Spending        16425
    ## 1265 Total.Health.Spending        16425
    ## 1266 Total.Health.Spending        16425
    ## 1267 Total.Health.Spending        16425
    ## 1268 Total.Health.Spending        17597
    ## 1269 Total.Health.Spending        17597
    ## 1270 Total.Health.Spending        17597
    ## 1271 Total.Health.Spending        17597
    ## 1272 Total.Health.Spending        17597
    ## 1273 Total.Health.Spending        17597
    ## 1274 Total.Health.Spending        17597
    ## 1275                  <NA>           NA
    ## 1276                  <NA>           NA
    ## 1277                  <NA>           NA
    ## 1278                  <NA>           NA
    ## 1279                  <NA>           NA
    ## 1280                  <NA>           NA
    ## 1281                  <NA>           NA
    ## 1282                  <NA>           NA
    ## 1283                  <NA>           NA
    ## 1284                  <NA>           NA
    ## 1285                  <NA>           NA
    ## 1286                  <NA>           NA
    ## 1287                  <NA>           NA
    ## 1288                  <NA>           NA
    ## 1289 Total.Health.Spending         6221
    ## 1290 Total.Health.Spending         6221
    ## 1291 Total.Health.Spending         6221
    ## 1292 Total.Health.Spending         6221
    ## 1293 Total.Health.Spending         6221
    ## 1294 Total.Health.Spending         6221
    ## 1295 Total.Health.Spending         6221
    ## 1296 Total.Health.Spending         6389
    ## 1297 Total.Health.Spending         6389
    ## 1298 Total.Health.Spending         6389
    ## 1299 Total.Health.Spending         6389
    ## 1300 Total.Health.Spending         6389
    ## 1301 Total.Health.Spending         6389
    ## 1302 Total.Health.Spending         6389
    ## 1303                  <NA>           NA
    ## 1304                  <NA>           NA
    ## 1305                  <NA>           NA
    ## 1306                  <NA>           NA
    ## 1307                  <NA>           NA
    ## 1308                  <NA>           NA
    ## 1309                  <NA>           NA
    ## 1310                  <NA>           NA
    ## 1311                  <NA>           NA
    ## 1312                  <NA>           NA
    ## 1313                  <NA>           NA
    ## 1314                  <NA>           NA
    ## 1315                  <NA>           NA
    ## 1316                  <NA>           NA
    ## 1317 Total.Health.Spending        60364
    ## 1318 Total.Health.Spending        60364
    ## 1319 Total.Health.Spending        60364
    ## 1320 Total.Health.Spending        60364
    ## 1321 Total.Health.Spending        60364
    ## 1322 Total.Health.Spending        60364
    ## 1323 Total.Health.Spending        60364
    ## 1324 Total.Health.Spending        62847
    ## 1325 Total.Health.Spending        62847
    ## 1326 Total.Health.Spending        62847
    ## 1327 Total.Health.Spending        62847
    ## 1328 Total.Health.Spending        62847
    ## 1329 Total.Health.Spending        62847
    ## 1330 Total.Health.Spending        62847
    ## 1331                  <NA>           NA
    ## 1332                  <NA>           NA
    ## 1333                  <NA>           NA
    ## 1334                  <NA>           NA
    ## 1335                  <NA>           NA
    ## 1336                  <NA>           NA
    ## 1337                  <NA>           NA
    ## 1338                  <NA>           NA
    ## 1339                  <NA>           NA
    ## 1340                  <NA>           NA
    ## 1341                  <NA>           NA
    ## 1342                  <NA>           NA
    ## 1343                  <NA>           NA
    ## 1344                  <NA>           NA
    ## 1345 Total.Health.Spending        53022
    ## 1346 Total.Health.Spending        53022
    ## 1347 Total.Health.Spending        53022
    ## 1348 Total.Health.Spending        53022
    ## 1349 Total.Health.Spending        53022
    ## 1350 Total.Health.Spending        53022
    ## 1351 Total.Health.Spending        53022
    ## 1352 Total.Health.Spending        55819
    ## 1353 Total.Health.Spending        55819
    ## 1354 Total.Health.Spending        55819
    ## 1355 Total.Health.Spending        55819
    ## 1356 Total.Health.Spending        55819
    ## 1357 Total.Health.Spending        55819
    ## 1358 Total.Health.Spending        55819
    ## 1359                  <NA>           NA
    ## 1360                  <NA>           NA
    ## 1361                  <NA>           NA
    ## 1362                  <NA>           NA
    ## 1363                  <NA>           NA
    ## 1364                  <NA>           NA
    ## 1365                  <NA>           NA
    ## 1366                  <NA>           NA
    ## 1367                  <NA>           NA
    ## 1368                  <NA>           NA
    ## 1369                  <NA>           NA
    ## 1370                  <NA>           NA
    ## 1371                  <NA>           NA
    ## 1372                  <NA>           NA
    ## 1373 Total.Health.Spending        16622
    ## 1374 Total.Health.Spending        16622
    ## 1375 Total.Health.Spending        16622
    ## 1376 Total.Health.Spending        16622
    ## 1377 Total.Health.Spending        16622
    ## 1378 Total.Health.Spending        16622
    ## 1379 Total.Health.Spending        16622
    ## 1380 Total.Health.Spending        17491
    ## 1381 Total.Health.Spending        17491
    ## 1382 Total.Health.Spending        17491
    ## 1383 Total.Health.Spending        17491
    ## 1384 Total.Health.Spending        17491
    ## 1385 Total.Health.Spending        17491
    ## 1386 Total.Health.Spending        17491
    ## 1387                  <NA>           NA
    ## 1388                  <NA>           NA
    ## 1389                  <NA>           NA
    ## 1390                  <NA>           NA
    ## 1391                  <NA>           NA
    ## 1392                  <NA>           NA
    ## 1393                  <NA>           NA
    ## 1394                  <NA>           NA
    ## 1395                  <NA>           NA
    ## 1396                  <NA>           NA
    ## 1397                  <NA>           NA
    ## 1398                  <NA>           NA
    ## 1399                  <NA>           NA
    ## 1400                  <NA>           NA
    ## 1401 Total.Health.Spending        47030
    ## 1402 Total.Health.Spending        47030
    ## 1403 Total.Health.Spending        47030
    ## 1404 Total.Health.Spending        47030
    ## 1405 Total.Health.Spending        47030
    ## 1406 Total.Health.Spending        47030
    ## 1407 Total.Health.Spending        47030
    ## 1408 Total.Health.Spending        50109
    ## 1409 Total.Health.Spending        50109
    ## 1410 Total.Health.Spending        50109
    ## 1411 Total.Health.Spending        50109
    ## 1412 Total.Health.Spending        50109
    ## 1413 Total.Health.Spending        50109
    ## 1414 Total.Health.Spending        50109
    ## 1415                  <NA>           NA
    ## 1416                  <NA>           NA
    ## 1417                  <NA>           NA
    ## 1418                  <NA>           NA
    ## 1419                  <NA>           NA
    ## 1420                  <NA>           NA
    ## 1421                  <NA>           NA
    ## 1422                  <NA>           NA
    ## 1423                  <NA>           NA
    ## 1424                  <NA>           NA
    ## 1425                  <NA>           NA
    ## 1426                  <NA>           NA
    ## 1427                  <NA>           NA
    ## 1428                  <NA>           NA
    ## 1429 Total.Health.Spending         4639
    ## 1430 Total.Health.Spending         4639
    ## 1431 Total.Health.Spending         4639
    ## 1432 Total.Health.Spending         4639
    ## 1433 Total.Health.Spending         4639
    ## 1434 Total.Health.Spending         4639
    ## 1435 Total.Health.Spending         4639
    ## 1436 Total.Health.Spending         4856
    ## 1437 Total.Health.Spending         4856
    ## 1438 Total.Health.Spending         4856
    ## 1439 Total.Health.Spending         4856
    ## 1440 Total.Health.Spending         4856
    ## 1441 Total.Health.Spending         4856
    ## 1442 Total.Health.Spending         4856
    ## 1443                  <NA>           NA
    ## 1444                  <NA>           NA
    ## 1445                  <NA>           NA
    ## 1446                  <NA>           NA
    ## 1447                  <NA>           NA
    ## 1448                  <NA>           NA
    ## 1449                  <NA>           NA
    ## 1450                  <NA>           NA
    ## 1451                  <NA>           NA
    ## 1452                  <NA>           NA
    ## 1453                  <NA>           NA
    ## 1454                  <NA>           NA
    ## 1455                  <NA>           NA
    ## 1456                  <NA>           NA

### 5. Submit a link to the repo “Lab2” via Canvas.
