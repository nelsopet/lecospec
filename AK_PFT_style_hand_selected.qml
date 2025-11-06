<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis minScale="1e+08" hasScaleBasedVisibilityFlag="0" version="3.28.11-Firenze" styleCategories="AllStyleCategories" maxScale="0">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal enabled="0" fetchMode="0" mode="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <elevation enabled="0" zscale="1" zoffset="0" symbology="Line" band="1">
    <data-defined-properties>
      <Option type="Map">
        <Option name="name" value="" type="QString"/>
        <Option name="properties"/>
        <Option name="type" value="collection" type="QString"/>
      </Option>
    </data-defined-properties>
    <profileLineSymbol>
      <symbol clip_to_extent="1" alpha="1" name="" force_rhr="0" type="line" is_animated="0" frame_rate="10">
        <data_defined_properties>
          <Option type="Map">
            <Option name="name" value="" type="QString"/>
            <Option name="properties"/>
            <Option name="type" value="collection" type="QString"/>
          </Option>
        </data_defined_properties>
        <layer enabled="1" pass="0" class="SimpleLine" locked="0">
          <Option type="Map">
            <Option name="align_dash_pattern" value="0" type="QString"/>
            <Option name="capstyle" value="square" type="QString"/>
            <Option name="customdash" value="5;2" type="QString"/>
            <Option name="customdash_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
            <Option name="customdash_unit" value="MM" type="QString"/>
            <Option name="dash_pattern_offset" value="0" type="QString"/>
            <Option name="dash_pattern_offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
            <Option name="dash_pattern_offset_unit" value="MM" type="QString"/>
            <Option name="draw_inside_polygon" value="0" type="QString"/>
            <Option name="joinstyle" value="bevel" type="QString"/>
            <Option name="line_color" value="232,113,141,255" type="QString"/>
            <Option name="line_style" value="solid" type="QString"/>
            <Option name="line_width" value="0.6" type="QString"/>
            <Option name="line_width_unit" value="MM" type="QString"/>
            <Option name="offset" value="0" type="QString"/>
            <Option name="offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
            <Option name="offset_unit" value="MM" type="QString"/>
            <Option name="ring_filter" value="0" type="QString"/>
            <Option name="trim_distance_end" value="0" type="QString"/>
            <Option name="trim_distance_end_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
            <Option name="trim_distance_end_unit" value="MM" type="QString"/>
            <Option name="trim_distance_start" value="0" type="QString"/>
            <Option name="trim_distance_start_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
            <Option name="trim_distance_start_unit" value="MM" type="QString"/>
            <Option name="tweak_dash_pattern_on_corners" value="0" type="QString"/>
            <Option name="use_custom_dash" value="0" type="QString"/>
            <Option name="width_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
          </Option>
          <data_defined_properties>
            <Option type="Map">
              <Option name="name" value="" type="QString"/>
              <Option name="properties"/>
              <Option name="type" value="collection" type="QString"/>
            </Option>
          </data_defined_properties>
        </layer>
      </symbol>
    </profileLineSymbol>
    <profileFillSymbol>
      <symbol clip_to_extent="1" alpha="1" name="" force_rhr="0" type="fill" is_animated="0" frame_rate="10">
        <data_defined_properties>
          <Option type="Map">
            <Option name="name" value="" type="QString"/>
            <Option name="properties"/>
            <Option name="type" value="collection" type="QString"/>
          </Option>
        </data_defined_properties>
        <layer enabled="1" pass="0" class="SimpleFill" locked="0">
          <Option type="Map">
            <Option name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
            <Option name="color" value="232,113,141,255" type="QString"/>
            <Option name="joinstyle" value="bevel" type="QString"/>
            <Option name="offset" value="0,0" type="QString"/>
            <Option name="offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
            <Option name="offset_unit" value="MM" type="QString"/>
            <Option name="outline_color" value="35,35,35,255" type="QString"/>
            <Option name="outline_style" value="no" type="QString"/>
            <Option name="outline_width" value="0.26" type="QString"/>
            <Option name="outline_width_unit" value="MM" type="QString"/>
            <Option name="style" value="solid" type="QString"/>
          </Option>
          <data_defined_properties>
            <Option type="Map">
              <Option name="name" value="" type="QString"/>
              <Option name="properties"/>
              <Option name="type" value="collection" type="QString"/>
            </Option>
          </data_defined_properties>
        </layer>
      </symbol>
    </profileFillSymbol>
  </elevation>
  <customproperties>
    <Option type="Map">
      <Option name="WMSBackgroundLayer" value="false" type="bool"/>
      <Option name="WMSPublishDataSourceUrl" value="false" type="bool"/>
      <Option name="embeddedWidgets/count" value="0" type="int"/>
      <Option name="identify/format" value="Value" type="QString"/>
    </Option>
  </customproperties>
  <pipe-data-defined-properties>
    <Option type="Map">
      <Option name="name" value="" type="QString"/>
      <Option name="properties"/>
      <Option name="type" value="collection" type="QString"/>
    </Option>
  </pipe-data-defined-properties>
  <pipe>
    <provider>
      <resampling enabled="false" maxOversampling="2" zoomedOutResamplingMethod="nearestNeighbour" zoomedInResamplingMethod="nearestNeighbour"/>
    </provider>
    <rasterrenderer alphaBand="-1" nodataColor="" opacity="1" type="paletted" band="1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <colorPalette>
        <paletteEntry alpha="255" color="#f3e8f6" value="0" label="Abiotic"/>
        <paletteEntry alpha="255" color="#88fd59" value="1" label="Forb"/>
        <paletteEntry alpha="255" color="#f4f726" value="2" label="Graminoid"/>
        <paletteEntry alpha="255" color="#4ffff3" value="3" label="Lichen"/>
        <paletteEntry alpha="255" color="#21f88d" value="4" label="Moss"/>
        <paletteEntry alpha="255" color="#02970a" value="5" label="ShrubDecid"/>
        <paletteEntry alpha="255" color="#265329" value="6" label="ShrubEvergreen"/>
        <paletteEntry alpha="255" color="#abdc32" value="7" label="TreeBroadleaf"/>
        <paletteEntry alpha="255" color="#012307" value="8" label="TreeEvergreen"/>
      </colorPalette>
      <colorramp name="[source]" type="gradient">
        <Option type="Map">
          <Option name="color1" value="68,1,84,255" type="QString"/>
          <Option name="color2" value="253,231,37,255" type="QString"/>
          <Option name="direction" value="ccw" type="QString"/>
          <Option name="discrete" value="0" type="QString"/>
          <Option name="rampType" value="gradient" type="QString"/>
          <Option name="spec" value="rgb" type="QString"/>
          <Option name="stops" value="0.0196078;70,8,92,255;rgb;ccw:0.0392157;71,16,99,255;rgb;ccw:0.0588235;72,23,105,255;rgb;ccw:0.0784314;72,29,111,255;rgb;ccw:0.0980392;72,36,117,255;rgb;ccw:0.117647;71,42,122,255;rgb;ccw:0.137255;70,48,126,255;rgb;ccw:0.156863;69,55,129,255;rgb;ccw:0.176471;67,61,132,255;rgb;ccw:0.196078;65,66,135,255;rgb;ccw:0.215686;63,72,137,255;rgb;ccw:0.235294;61,78,138,255;rgb;ccw:0.254902;58,83,139,255;rgb;ccw:0.27451;56,89,140,255;rgb;ccw:0.294118;53,94,141,255;rgb;ccw:0.313725;51,99,141,255;rgb;ccw:0.333333;49,104,142,255;rgb;ccw:0.352941;46,109,142,255;rgb;ccw:0.372549;44,113,142,255;rgb;ccw:0.392157;42,118,142,255;rgb;ccw:0.411765;41,123,142,255;rgb;ccw:0.431373;39,128,142,255;rgb;ccw:0.45098;37,132,142,255;rgb;ccw:0.470588;35,137,142,255;rgb;ccw:0.490196;33,142,141,255;rgb;ccw:0.509804;32,146,140,255;rgb;ccw:0.529412;31,151,139,255;rgb;ccw:0.54902;30,156,137,255;rgb;ccw:0.568627;31,161,136,255;rgb;ccw:0.588235;33,165,133,255;rgb;ccw:0.607843;36,170,131,255;rgb;ccw:0.627451;40,174,128,255;rgb;ccw:0.647059;46,179,124,255;rgb;ccw:0.666667;53,183,121,255;rgb;ccw:0.686275;61,188,116,255;rgb;ccw:0.705882;70,192,111,255;rgb;ccw:0.72549;80,196,106,255;rgb;ccw:0.745098;90,200,100,255;rgb;ccw:0.764706;101,203,94,255;rgb;ccw:0.784314;112,207,87,255;rgb;ccw:0.803922;124,210,80,255;rgb;ccw:0.823529;137,213,72,255;rgb;ccw:0.843137;149,216,64,255;rgb;ccw:0.862745;162,218,55,255;rgb;ccw:0.882353;176,221,47,255;rgb;ccw:0.901961;189,223,38,255;rgb;ccw:0.921569;202,225,31,255;rgb;ccw:0.941176;216,226,25,255;rgb;ccw:0.960784;229,228,25,255;rgb;ccw:0.980392;241,229,29,255;rgb;ccw" type="QString"/>
        </Option>
      </colorramp>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="0" gamma="1"/>
    <huesaturation invertColors="0" colorizeStrength="100" saturation="0" colorizeRed="255" colorizeBlue="128" colorizeOn="0" grayscaleMode="0" colorizeGreen="128"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
