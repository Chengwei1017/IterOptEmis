# An iteratively optimized downscaling method for city-scale air quality forecast emission inventory establishment
## Supplementary Materials

* Source codes:
1. Source Code of FMEmT.R  
This is the source code for the Fast Model-ready Emission Tool used in the paper, as a replacement of SMOKE model to process emission files into netcdf format for CMAQ model.  
FMEmT排放清单处理工具的源代码，可替代SMOKE模型进行模型排放清单的处理，生成适用于CMAQ模型的NetCDF格式排放清单数据。  
2. Source Code of Fast Iterative Optimization.R  
This is the source code for the Fast Iterative Optimization method used in the paper.  
论文中进行排放清单快速迭代优化的程序源代码。  
3. Source code RegionMapper.R  
This is the source code for the RegionMapper used to create the region file, as the format of ISAM or DDM region file.
用于创建映射观测数据和模型网格的区域映射文件的源代码，该文件也可通过spatial allocator创建，与ISAM或DDM所需的区域映射文件格式相同。
4. Original MEIC-HR.7z  
This is the original MEIC-HR emission for the CP8C regions including two domains. The emission files were processed from MEIC-HR 2017 released by the MEIC Team of Tsinghua University.  
基于清华大学MEIC-HR 2017排放数据进行网格化后的原始排放清单。  
5. Optimized emission for the CP8C region.7z  
This is the optimized emission files for the CP8C regions.  
通过论文迭代优化方法处理后的排放清单。  
6. Obs_template.xlsx  
This is an example for how to create observation files in xlsx format.  
实测数据模板。
7. RUN_EMIS_OPT.csh  
This is a sample script to run the optimization.   
运行迭代优化的参考脚本。  
* Citation  
https://doi.org/10.1016/j.scitotenv.2024.176824

* Contact  
We hope our work helps, for any questions or suggestions, please contact my supervisor: fmyang#scu.edu.cn, or me: lcw#cdaes.cn.
