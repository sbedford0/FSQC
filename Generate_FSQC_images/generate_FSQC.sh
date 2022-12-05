#!/bin/bash -l
#

sub=$1 #participant ID
indir=$2 #path to input directory
outdir=$3 #path to output folder for images
tmpdir=$4 #path to temporary directory where command files will be generated
# load all defaults

module load freesurfer/7.1.0

qcdir=$outdir
dataDir=$indir
mkdir -p ${qcdir}

cd ${dataDir}/${sub}
sesslist=$(find * -maxdepth 0 -type d -name 'ses*')

for ses in ${sesslist} ; do
  cd ${dataDir}/${sub}/${ses}
  runlist=$(find * -maxdepth 0 -type d -name 'run*')
  for run in ${runlist} ; do

    basedir=${dataDir}/${sub}/${ses}/${run}
        if [ -d ${basedir}/surfaces/${sub}/mri ]; then
          if [ ! -f ${qcdir}/${sub}_${ses}_${run}_cor1_surfaces.png ]; then
            surfdir=${dataDir}/${sub}/${ses}/${run}/surfaces/${sub}
            # set processing directories
            xdgDir=${tmpdir}/${sub}_${ses}_${run}/xdg
            mkdir -p ${xdgDir}
            export XDG_RUNTIME_DIR=$xdgDir
            cd ${xdgDir}
            echo freeview -v ${surfdir}/mri/brain.mgz -f ${surfdir}/surf/lh.pial:edgecolor=red:edgethickness=1 -f ${surfdir}/surf/rh.pial:edgecolor=red:edgethickness=1 -f ${surfdir}/surf/lh.white:edgecolor=blue:edgethickness=1 -f ${surfdir}/surf/rh.white:edgecolor=blue:edgethickness=1 > freeviewshot_multi.txt
            echo -viewport coronal -slice 102 128 100 -ss ${sub}_${ses}_${run}_cor1_surfaces.png 2 1 -noquit >> freeviewshot_multi.txt
            echo -viewport coronal -slice 102 128 128 -ss ${sub}_${ses}_${run}_cor2_surfaces.png 2 1 -noquit >> freeviewshot_multi.txt
            echo -viewport coronal -slice 102 128 152 -ss ${sub}_${ses}_${run}_cor3_surfaces.png 2 1 -noquit >> freeviewshot_multi.txt
            echo -viewport sagittal -slice 82 128 128 -ss ${sub}_${ses}_${run}_sag1_surfaces.png 2 1 -noquit >> freeviewshot_multi.txt
            echo -viewport sagittal -slice 102 128 128 -ss ${sub}_${ses}_${run}_sag2_surfaces.png 2 1 -noquit >> freeviewshot_multi.txt
            echo -viewport sagittal -slice 152 128 128 -ss ${sub}_${ses}_${run}_sag3_surfaces.png 2 1 -noquit >> freeviewshot_multi.txt
            echo -viewport sagittal -slice 172 128 128 -ss ${sub}_${ses}_${run}_sag4_surfaces.png 2 1 -noquit >> freeviewshot_multi.txt
            echo -viewport axial -slice 102 100 128 -ss ${sub}_${ses}_${run}_ax1_surfaces.png 2 1 -noquit >> freeviewshot_multi.txt
            echo -viewport axial -slice 102 120 128 -ss ${sub}_${ses}_${run}_ax2_surfaces.png 2 1 -noquit >> freeviewshot_multi.txt
            echo -viewport axial -slice 102 140 128 -ss ${sub}_${ses}_${run}_ax3_surfaces.png 2 1 -quit >> freeviewshot_multi.txt
            cd ${qcdir}
            echo "creating QC image for" ${sub}
            xvfb-run -a --server-args="-screen 0 512x384x16" freeview -cmd ${xdgDir}/freeviewshot_multi.txt
            rm -R ${tmpdir}/${sub}_${ses}_${run}/

          fi
        fi
  done
done
