pro modis_sixs_lut
  igeom = 0
  sz = [0, 78]
  sz = interpol(sz, 14)
  sa = [0, 180]
  sa = interpol(sa, 16)
  vz = [0, 78]
  vz = interpol(vz, 14)
  va = 0
  month = 9
  day = 1
  idatm = 2
  iaer = 2
  v = 0
  aod = [0.01, 0.25, 0.5, 1.0, 1.25, 1.5, 2.0, 3.0, 5.0]
  xps = 0
  xpp = -1000
  iwave = 42
  inhomo = 0
  idirec = 0
  igroun = 1
  rapp = -2
  openw, 2, 'V:/Softwares/6SV-1.1/SixS/results/modis_red_lut.txt', width = 8000
  for aod_i = 0, n_elements(aod) - 1 do begin
    for sz_i = 0, n_elements(sz) - 1 do begin
      for vz_i = 0, n_elements(vz) - 1 do begin
        for sa_i = 0, n_elements(sa) - 1 do begin
          openw, 1, 'V:/Softwares/6SV-1.1/SixS/results/In.txt'
          printf, 1, igeom
          printf, 1, sz[sz_i]
          printf, 1, sa[sa_i]
          printf, 1, vz[vz_i]
          printf, 1, va
          printf, 1, month
          printf, 1, day
          printf, 1, idatm
          printf, 1, iaer
          printf, 1, v
          printf, 1, aod[aod_i]
          printf, 1, xps
          printf, 1, xpp
          printf, 1, iwave
          printf, 1, inhomo
          printf, 1, idirec
          printf, 1, igroun
          printf, 1, rapp
          free_lun, 1
          spawn, 'V:/Softwares/6SV-1.1/SixS/envs/sixs.exe<V:/Softwares/6SV-1.1/SixS/results/In.txt>V:/Softwares/6SV-1.1/SixS/results/Out.txt', /hide
          openr, 1, 'V:/Softwares/6SV-1.1/SixS/results/Out.txt'
          temp_str = ''
          skip_lun,1,123,/lines
          readf, 1, temp_str
          temp_str_spl = strsplit(temp_str, /extract)
          out_T = temp_str_spl[7]
          skip_lun,1,5,/lines
          readf, 1, temp_str
          temp_str_spl = strsplit(temp_str, /extract)
          out_S = temp_str_spl[6]
          skip_lun,1,2,/lines
          readf, 1, temp_str
          temp_str_spl = strsplit(temp_str, /extract)
          out_rou = temp_str_spl[6]
          ;print, out_rou
          free_lun, 1
          printf, 2, [out_rou, out_T, out_S,  sz[sz_i], vz[vz_i], sa[sa_i], aod[aod_i]]
        endfor
       endfor
    endfor 
  endfor
  free_lun, 2
end