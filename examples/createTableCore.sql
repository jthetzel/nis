create table `core` (
`age` char (3),
`ageday` char (3),
`amonth` char (2),
`asource` char (2),
`asource_x` char (8),
`asourceub92` char (1),
`atype` char (2),
`aweekend` char (2),
`died` char (2),
`discwt` char (11),
`discwtcharge` char (10),
`dispub04` char (2),
`dispub92` char (2),
`dispuniform` char (2),
`dqtr` char (2),
`dqtr_x` char (2),
`drg` char (3),
`drg_nopoa` char (3),
`drg10` char (3),
`drg18` char (3),
`drg24` char (3),
`drgver` char (2),
`dshospid` char (17),
`dx1` char (5),
`dx10` char (5),
`dx11` char (5),
`dx12` char (5),
`dx13` char (5),
`dx14` char (5),
`dx15` char (5),
`dx16` char (5),
`dx17` char (5),
`dx18` char (5),
`dx19` char (5),
`dx2` char (5),
`dx20` char (5),
`dx21` char (5),
`dx22` char (5),
`dx23` char (5),
`dx24` char (5),
`dx25` char (5),
`dx3` char (5),
`dx4` char (5),
`dx5` char (5),
`dx6` char (5),
`dx7` char (5),
`dx8` char (5),
`dx9` char (5),
`dxccs1` char (4),
`dxccs10` char (4),
`dxccs11` char (4),
`dxccs12` char (4),
`dxccs13` char (4),
`dxccs14` char (4),
`dxccs15` char (4),
`dxccs16` char (3),
`dxccs17` char (3),
`dxccs18` char (3),
`dxccs19` char (3),
`dxccs2` char (4),
`dxccs20` char (3),
`dxccs21` char (3),
`dxccs22` char (3),
`dxccs23` char (3),
`dxccs24` char (3),
`dxccs25` char (3),
`dxccs3` char (4),
`dxccs4` char (4),
`dxccs5` char (4),
`dxccs6` char (4),
`dxccs7` char (4),
`dxccs8` char (4),
`dxccs9` char (4),
`e_ccs1` char (4),
`e_ccs2` char (4),
`e_ccs3` char (4),
`e_ccs4` char (4),
`ecode1` char (5),
`ecode2` char (5),
`ecode3` char (5),
`ecode4` char (5),
`elective` char (2),
`female` char (2),
`hcup_ed` char (3),
`hospbrth` char (2),
`hospid` char (5),
`hospst` char (2),
`hospstco` char (5),
`key` char (14),
`los` char (5),
`los_x` char (6),
`mdc` char (2),
`mdc_nopoa` char (2),
`mdc10` char (2),
`mdc18` char (2),
`mdc24` char (2),
`mdid_s` char (16),
`mdnum1_r` char (5),
`mdnum1_s` char (16),
`mdnum2_r` char (5),
`mdnum2_s` char (16),
`nchronic` char (2),
`ndx` char (2),
`necode` char (3),
`neomat` char (2),
`nis_stratum` char (4),
`npr` char (2),
`orproc` char (2),
`pay1` char (2),
`pay1_x` char (10),
`pay2` char (2),
`pay2_x` char (10),
`pl_nchs2006` char (3),
`pl_ur_cat4` char (2),
`pointoforigin_x` char (8),
`pointoforiginub04` char (1),
`pr1` char (4),
`pr10` char (4),
`pr11` char (4),
`pr12` char (4),
`pr13` char (4),
`pr14` char (4),
`pr15` char (4),
`pr2` char (4),
`pr3` char (4),
`pr4` char (4),
`pr5` char (4),
`pr6` char (4),
`pr7` char (4),
`pr8` char (4),
`pr9` char (4),
`prccs1` char (3),
`prccs10` char (3),
`prccs11` char (3),
`prccs12` char (3),
`prccs13` char (3),
`prccs14` char (3),
`prccs15` char (3),
`prccs2` char (3),
`prccs3` char (3),
`prccs4` char (3),
`prccs5` char (3),
`prccs6` char (3),
`prccs7` char (3),
`prccs8` char (3),
`prccs9` char (3),
`prday1` char (3),
`prday10` char (3),
`prday11` char (3),
`prday12` char (3),
`prday13` char (3),
`prday14` char (3),
`prday15` char (3),
`prday2` char (3),
`prday3` char (3),
`prday4` char (3),
`prday5` char (3),
`prday6` char (3),
`prday7` char (3),
`prday8` char (3),
`prday9` char (3),
`race` char (2),
`surgid_s` char (16),
`totchg` char (10),
`totchg_x` char (15),
`tran_in` char (2),
`year` char (4),
`zipinc` char (2),
`zipinc_qrtl` char (2)
);

create table `dx` (
`key` char (14),
`icd9` char (5),
`variable` char (4)
);

create table `pr` (
`key` char (14),
`icd9` char (4),
`variable` char (4)
);