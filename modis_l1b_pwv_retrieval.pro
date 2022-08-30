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

pro modis_l1b_pwv_retrieval
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init
  modis_l1b_file = '\\Mac\Home\Desktop\AOD_Retrival\DATA\MOD021KM\MOD021KM.A2022191.0200.061.2022191131722.hdf'
  cloud_file = '\\Mac\Home\Desktop\AOD_Retrival\DATA\MOD021KM\MOD35_L2.A2022191.0200.061.2022191131937.hdf'
  result_name = '\\Mac\Home\Desktop\AOD_Retrival\DATA\Results\MOD021KM.A2022191.0200.061.2022191131722_PWV_GEO.tiff'
  output_directory = '\\Mac\Home\Desktop\AOD_Retrival\DATA\Results\'
  km_ref = ref_rad_cal(modis_l1b_file, 'EV_1KM_RefSB', 'reflectance_scales', 'reflectance_offsets')
  hkm_ref = ref_rad_cal(modis_l1b_file, 'EV_500_Aggr1km_RefSB', 'reflectance_scales', 'reflectance_offsets')
  qkm_ref = ref_rad_cal(modis_l1b_file, 'EV_250_Aggr1km_RefSB', 'reflectance_scales', 'reflectance_offsets')
  
  modis_lon_data = hdf4_data_get(modis_l1b_file, 'Longitude')
  modis_lat_data = hdf4_data_get(modis_l1b_file, 'Latitude')
  
  ref_band2 = qkm_ref[*, *, 1]
  ref_band5 = hkm_ref[*, *, 2]
  ref_band17 = km_ref[*, *, 11]
  ref_band17 = km_ref[*, *, 11]
  ref_band18 = km_ref[*, *, 12]
  ref_band19 = km_ref[*, *, 13]
  km_ref = !null
  hkm_ref = !null
  km_ref = !null
  
  cloud_data = hdf4_data_get(cloud_file, 'Cloud_Mask')
  cloud_0 = cloud_data[*, *, 0]
  cloud_0 = (cloud_0 ge 0) * cloud_0 + (cloud_0 lt 0) * (128 + abs(cloud_0))
  cloud_0_size = size(cloud_0)

  cloud_binary = bytarr(cloud_0_size[1], cloud_0_size[2], 8)
  for cloud_i = 0, 7 do begin
    cloud_binary[*, *, cloud_i] = cloud_0 mod 2
    cloud_0 = cloud_0 / 2
  endfor
  help, cloud_binary
  cloud_result = (cloud_binary[*, *, 0] eq 1) and (cloud_binary[*, *, 1] eq 0) and (cloud_binary[*, *, 2] eq 0)
  clear_result = cloud_result eq 0
  
  ICIBR_band17 = ref_band17 / (0.876 * ref_band2 + 0.124 * ref_band5)
  ICIBR_band18 = ref_band18 / (0.795 * ref_band2 + 0.205 * ref_band5)
  ICIBR_band19 = ref_band19 / (0.796 * ref_band2 + 0.204 * ref_band5)
  PWV_band17 = (0.020 - 1.013 * alog(ICIBR_band17))^2.0
  PWV_band18 = (0.022 - 1.077 * alog(ICIBR_band18))^2.0
  PWV_band19 = (0.027 - 1.334 * alog(ICIBR_band19))^2.0
  
  PWV=0.326 * PWV_band17 + 0.396 * PWV_band18 + 0.556 * PWV_band19
  modis_target_data = PWV * clear_result
  
  target_data_size = size(modis_target_data)
  
  modis_lon_data = congrid(modis_lon_data, target_data_size[1], target_data_size[2], /interp)
  modis_lat_data = congrid(modis_lat_data, target_data_size[1], target_data_size[2], /interp)
  
  out_lon = output_directory + 'lon_out.tiff'
  out_lat = output_directory + 'lat_out.tiff'
  out_target = output_directory + 'target.tiff'
  write_tiff, out_lon, modis_lon_data, /float
  write_tiff, out_lat, modis_lat_data, /float
  write_tiff, out_target, modis_target_data, /float
  
  envi_open_file, out_lon, r_fid = x_fid ; 打开经度数据，获取经度文件id
  envi_open_file, out_lat, r_fid = y_fid ; 打开纬度数据，获取纬度文件id
  envi_open_file, out_target, r_fid = target_fid ; 打开目标数据，获取目标文件id
  
  out_name_glt = output_directory + file_basename(modis_l1b_file, '.hdf') + '_glt.img'
  out_name_glt_hdr = output_directory + file_basename(modis_l1b_file, '.hdf') + '_glt.hdr'
  i_proj = envi_proj_create(/geographic)
  o_proj = envi_proj_create(/geographic)
  envi_glt_doit, $
    i_proj = i_proj, x_fid = x_fid, y_fid = y_fid, x_pos = 0, y_pos = 0, $ ;指定创建GLT所需输入信息
    o_proj = o_proj,pixel_size = pixel_size, rotation = 0.0, out_name = out_name_glt, r_fid = glt_fid ; 指定输出GLT文件信息
    
  out_name_geo = output_directory + file_basename(modis_l1b_file, '.hdf') + '_georef.img'
  out_name_geo_hdf = output_directory + file_basename(modis_l1b_file, '.hdf') + '_georef.hdf'
  envi_georef_from_glt_doit, $
    glt_fid = glt_fid, $ ;指定重投影所需GLT文件信息
    fid = target_fid, pos = 0, $ ;指定待投影数据id
    out_name = out_name_geo, r_fid = geo_fid ; 指定输出重投影文件信息
  envi_file_query, geo_fid, dims = data_dims
  target_data = envi_get_data(fid = geo_fid, pos = 0,dims = data_dims)
  
  map_info = envi_get_map_info(fid = geo_fid)
  geo_loc = map_info.(1)
  px_size = map_info.(2)
  
  geo_info = {$
    MODELPIXELSCALETAG:[px_size[0], px_size[1], 0.0], $
    MODELTIEPOINTTAG:[0.0, 0.0, 0.0, geo_loc[2], geo_loc[3],0.0], $
    GTMODELTYPEGEOKEY:2, $
    GTRASTERTYPEGEOKEY:1, $
    GEOGRAPHICTYPEGEOKEY:4326, $
    GEOGCITATIONGEOKEY:'GCS_WGS_1984', $
    GEOGANGULARUNITSGEOKEY:9102, $
    GEOGSEMIMAJORAXISGEOKEY:6378137.0, $
    GEOGINVFLATTENINGGEOKEY:298.25722}
    
  write_tiff, result_name, target_data, /float, geotiff = geo_info
  
  envi_file_mng, id = x_fid, /remove
  envi_file_mng, id = y_fid, /remove
  envi_file_mng, id = target_fid, /remove
  envi_file_mng, id = glt_fid, /remove
  envi_file_mng, id = geo_fid, /remove
  file_delete, [out_lon, out_lat, out_target, out_name_glt, out_name_glt_hdr, out_name_geo, out_name_geo_hdr]
  envi_batch_exit, /no_confirm
  
end