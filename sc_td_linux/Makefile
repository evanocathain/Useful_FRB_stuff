LOCAL_ROOT = .
include $(LOCAL_ROOT)/include/Makefile.standard

#LDFLAGS += -lgenutil

INCLUDES = sc_td.inc sc_pmhead.inc

PROGRAMS   = sc_td_new.f pmhdr_edt.f 
#tapetest.f 

sc_td_new.srcs = st_init.f st_inpar.f st_topen.f st_rdblk.f st_gethdr.f st_fopen.f \
  st_trans.f st_fclose.f sct_$(LOGIN_ARCH).f repair_hdr.f str_coord.f \
  sla_djcl.f ubd128_$(LOGIN_ARCH).c exstat.f sc_rblk.c coord_str.f \
  st_goodpsr.f st_scfclose.f

ifneq (${LOGIN_ARCH},linux)
  PROGRAMS += sc_tt_new.f
  sc_tt_new.srcs = sct_$(LOGIN_ARCH).f
endif

ifeq (${LOGIN_ARCH},linux) 
  sc_td_new.srcs += tapefn.c
endif

include $(LOCAL_ROOT)/include/Makefile.extended

