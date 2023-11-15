#!/bin/bash
#SBATCH -A feczk001
#SBATCH --time=1:00:00
#SBATCH --ntasks=1
#SBATCH --mem=32g
#SBATCH --tmp=32g
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=and02709@umn.edu

# *** Make sure you have a new enough getopt to handle long options (see the man page)
getopt -T &>/dev/null
if [[ $? -ne 4 ]]; then echo "Getopt is too old!" >&2 ; exit 1 ; fi

declare {setwd,xfile,yfile,npc,nfolds,sparams,stype,kernel,niter,trace,balance}
OPTS=$(getopt -u -o '' -a --longoptions 'setwd:,xfile:,yfile:,npc:,nfolds:,sparams:,stype:,kernel:,niter:,trace:,balance:' -n "$0" -- "$@")
    # *** Added -o '' ; surrounted the longoptions by ''
if [[ $? -ne 0 ]] ; then echo "Failed parsing options." >&2 ; exit 1 ; fi
    # *** This has to be right after the OPTS= assignment or $? will be overwritten

set -- $OPTS
    # *** As suggested by chepner

while true; do
  case $1 in
	--setwd )
		setwd=$2
		shift 2
		;;
	--xfile )
        	xfile=$2
        	shift 2
        	;;
	--yfile )
        	yfile=$2
        	shift 2
        	;;
	--npc )
        	npc=$2
        	shift 2
        	;;
	--nfolds )
        	nfolds=$2
        	shift 2
        	;;
	--sparams )
		sparams=$2
		shift 2
		;;
	--stype )
		stype=$2
		shift 2
		;;
	--kernel )
		kernel=$2
		shift 2
		;;
	--niter )
		niter=$2
		shift 2
		;;
	--trace )
		trace=$2
		shift 2
		;;
	--balance )
		balance=$2
		shift 2
		;;
	--)
        	shift
        	break
        	;;
    *)
  esac
done
module load R/4.3.0-openblas
Rscript ${setwd}prelim_data_sspca_param.R $setwd $xfile $yfile $npc $nfolds $sparams $stype $kernel $niter $trace $balance
exit 0
