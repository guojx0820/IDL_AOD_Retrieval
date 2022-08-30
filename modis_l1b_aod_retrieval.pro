function hdf4_data_get, file_name, sds_name
  sd_id = hdf_sd_start(file_name, /read)
  sds_index = hdf_sd_nametoindex(sd_id, sds_name)
  sds_id = hdf_sd_select(sd_id, sds_index)
  hdf_sd_getdata, sds_id,data
  hdf_sd_endaccess, sds_id
  hdf_sd_end, sd_id
  return, data
end

;Radiometric Calibration
function ref_rad_cal, file_name, sds_name, scale_name, offset_name
  sd_id = hdf_sd_start(file_name, /read)
  sds_index = hdf_sd_nametoindex(sd_id, sds_name)
  sds_id = hdf_sd_select(sd_id, sds_index)
  hdf_sd_getdata,sds_id,data
  att_index = hdf_sd_attrfind(sds_id, scale_name)
  hdf_sd_attrinfo, sds_id, att_index, data = scale_data
  att_index = hdf_sd_attrfind(sds_id, offset_name)
  hdf_sd_attrinfo, sds_id, att_index, data = offset_data
  hdf_sd_endaccess, sds_id
  hdf_sd_end, sd_id
  data_size = size(data)
  data_ref = fltarr(data_size[1], data_size[2], data_size[3])
  ;print, data_ref
  for layer_i = 0, data_size[3]-1 do begin
    data_ref[*, *, layer_i] = scale_data[layer_i] * (data[*, *, layer_i] - offset_data[layer_i])
  endfor
  data = !null
  return, data_ref
end

pro modis_l1b_aod_retrieval
  modis_l1b_file = '\\Mac\Home\Desktop\AOD_Retrival\DATA\MOD021KM\MOD021KM.A2022191.0200.061.2022191131722.hdf'
  cloud_file = '\\Mac\Home\Desktop\AOD_Retrival\DATA\MOD021KM\MOD35_L2.A2022191.0200.061.2022191131937.hdf'
  output_directory = '\\Mac\Home\Desktop\AOD_Retrival\DATA\Results\'
  red_lut_file = '\\Mac\Home\Desktop\AOD_Retrival\LUT\modis_red_lut.txt'
  blue_lut_file = '\\Mac\Home\Desktop\AOD_Retrival\LUT\modis_blue_lut.txt'
  hkm_ref = ref_rad_cal(modis_l1b_file, 'EV_500_Aggr1km_RefSB', 'reflectance_scales', 'reflectance_offsets')
  qkm_ref = ref_rad_cal(modis_l1b_file, 'EV_250_Aggr1km_RefSB', 'reflectance_scales', 'reflectance_offsets')
  km_ems = ref_rad_cal(modis_l1b_file, 'EV_1KM_Emissive', 'radiance_scales', 'radiance_offsets')
  ref_size = size(hkm_ref)
  ;ndvi = (qkm_ref[*, *, 1] - qkm_ref[*, *, 0]) / (qkm_ref[*, *, 1] + qkm_ref[*, *, 0])
  ;角度读取
  sz_angle = congrid(hdf4_data_get(modis_l1b_file, 'SolarZenith'), ref_size[1], ref_size[2], /interp) * 0.01
  vz_angle = congrid(hdf4_data_get(modis_l1b_file, 'SensorZenith'), ref_size[1], ref_size[2], /interp) * 0.01
  sa_angle = congrid(hdf4_data_get(modis_l1b_file, 'SolarAzimuth'), ref_size[1], ref_size[2], /interp) * 0.01
  va_angle = congrid(hdf4_data_get(modis_l1b_file, 'SensorAzimuth'), ref_size[1], ref_size[2], /interp) * 0.01
  ra_angle = abs(sa_angle - va_angle)
  ra_angle = (ra_angle le 180.0) * ra_angle + (ra_angle gt 180.0) * (360.0 - ra_angle)
  sca_angle_cos = -(cos(vz_angle * !DTOR) * cos(sz_angle * !DTOR) + sin(vz_angle * !DTOR) * sin(sz_angle * !DTOR) * cos(ra_angle * !DTOR))
  sca_angle = acos(sca_angle_cos) / !DTOR

  red_ref = qkm_ref[*, *, 0] / cos(sz_angle * !DTOR)
  blue_ref = hkm_ref[*, *, 0] / cos(sz_angle * !DTOR)
  ir_ref = hkm_ref[*, *, 2] / cos(sz_angle * !DTOR)
  fr_ref = hkm_ref[*, *, 4] / cos(sz_angle * !DTOR)
  hkm_ref = !null
  qkm_ref = !null
  
  cloud_data = hdf4_data_get(cloud_file, 'Cloud_Mask')
  cloud_0 = cloud_data[*, *, 0]
  cloud_0 = (cloud_0 ge 0) * cloud_0 + (cloud_0 lt 0) * (128 + abs(cloud_0))
  cloud_0_size = size(cloud_0)
  
  cloud_binary = bytarr(cloud_0_size[1], cloud_0_size[2], 8)
  for cloud_i = 0, 7 do begin
    cloud_binary[*, *, cloud_i] = cloud_0 mod 2
    cloud_0 = cloud_0 / 2
  endfor

  cloud_result = (cloud_binary[*, *, 0] eq 1) and (cloud_binary[*, *, 1] eq 0) and (cloud_binary[*, *, 2] eq 0)
  ; 地表反射率估算
  ndvi_swir = (ir_ref - fr_ref) / (ir_ref + fr_ref)
  slope = (ndvi_swir lt 0.25) * 0.48 + $
    (ndvi_swir gt 0.75) * 0.58 + $
    ((ndvi_swir ge 0.25) and (ndvi_swir le 0.75)) * (0.48 + 0.2 * (ndvi_swir - 0.25))

  slope = slope + 0.002 * sca_angle - 0.27
  yint = -0.00025 * sca_angle + 0.033
  p_red = fr_ref * slope +yint
  p_blue = p_red * 0.49 +0.005
  
  ;读取查找表
  ;红波段
  openr, 1, red_lut_file
  red_lut_data = fltarr(7, file_lines(red_lut_file))
  readf, 1, red_lut_data
  free_lun, 1
  
  ;蓝波段
  openr, 1, blue_lut_file
  blue_lut_data = fltarr(7, file_lines(blue_lut_file))
  readf, 1, blue_lut_data
  free_lun, 1
  
  sz = [0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78]
  sz_n = n_elements(sz)
  vz = [0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78]
  vz_n = n_elements(vz)
  ra = [0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180]
  ra_n = n_elements(ra)
  aod = [0.01, 0.25, 0.5, 1.0, 1.25, 1.5, 2.0, 3.0, 5.0]
  aod_n = n_elements(aod)
  red_aod = fltarr(ref_size[1], ref_size[2])
  
  
  for line_i = 0, ref_size[2] - 1 do begin
    for col_i = 0, ref_size[1] - 1 do begin
      if (cloud_result[col_i, line_i] eq 1) or (ir_ref[col_i, line_i] gt 0.25) or (ir_ref[col_i, line_i] lt 0.01) then continue
      aod_lut_subset = fltarr(7, aod_n)
      sz_temp = sz_angle[col_i, line_i]
      vz_temp = vz_angle[col_i, line_i]
      ra_temp = ra_angle[col_i, line_i]
      
      ra_dif = abs(ra_temp - ra)
      vz_dif = abs(vz_temp - vz)
      sz_dif = abs(sz_temp - sz)
      
      ra_pos = where(ra_dif eq min(ra_dif))
      vz_pos = where(vz_dif eq min(vz_dif))
      sz_pos = where(sz_dif eq min(sz_dif))

      for aod_i = 0, aod_n-1 do begin
        line_pos = ra_pos[0] + vz_pos[0] * ra_n + sz_pos[0] * ra_n * vz_n + aod_i * ra_n * vz_n * sz_n
        aod_lut_subset[*, aod_i] = red_lut_data[*, line_pos]
      endfor
      ;模拟toa和卫星实测toa对比作差
      toa_sim = aod_lut_subset[0, *] + (aod_lut_subset[1, *] * p_red[col_i, line_i]) / (1.0 - aod_lut_subset[2, *] * p_red[col_i, line_i])
      delta_toa = toa_sim - red_ref[col_i, line_i]
      ;找出toa差值最接近零的一正一负的toa做两点内插
      gt0_pos = where(delta_toa gt 0)
      lt0_pos = where(delta_toa lt 0)
      ;如果找不到则跳出
      if (gt0_pos[0] eq -1) or (lt0_pos[0] eq -1) then continue
      ;找到两个最接近零的数
      max_neg_delta = max(delta_toa[lt0_pos])
      min_posi_delta = min(delta_toa[gt0_pos])
      ;找到两个最接近零的数的下标
      max_neg_pos = where(delta_toa eq max_neg_delta)
      min_posi_pos = where(delta_toa eq min_posi_delta)
      ;按照下标找到对应的aod值
      max_neg_aod = aod_lut_subset[6, max_neg_pos]
      min_posi_aod = aod_lut_subset[6, min_posi_pos]
      ;利用两点式求斜率
      fit_k = (min_posi_aod - max_neg_aod) / (min_posi_delta - max_neg_delta)
      aod_interp = fit_k * (0.0 - max_neg_delta) + min_posi_aod
      red_aod[col_i, line_i] = aod_interp
      
    endfor
  endfor
  red_aod = (red_aod gt 0) * red_aod
  write_tiff, output_directory + 'aod_red.tiff', red_aod, /float
end

;clear_land_result = clear_result and (cloud_binary[*, *, 6] eq 1) and (cloud_binary[*, *, 7] eq 1)
;ndvi_clear = ndvi * clear_result
;ndvi_land_clear = ndvi * clear_land_result
;write_tiff, '\\Mac\Home\Desktop\AOD_Retrival\DATA\Results\clear_result.tiff', clear_result
;write_tiff, '\\Mac\Home\Desktop\AOD_Retrival\DATA\Results\ndvi.tiff', ndvi,/float
;write_tiff, '\\Mac\Home\Desktop\AOD_Retrival\DATA\Results\ndvi_clear.tiff', ndvi_clear,/float
;write_tiff, '\\Mac\Home\Desktop\AOD_Retrival\DATA\Results\ndvi_land_clear.tiff', ndvi_land_clear,/float
