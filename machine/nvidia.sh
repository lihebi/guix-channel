modprobe ipmi_devintf
# sudo modprobe drm_kms_helper
# modprobe ipmi_msghandler
insmod /lib/modules/5.0.1-gnu/video/nvidia.ko
insmod /lib/modules/5.0.1-gnu/video/nvidia-modeset.ko
insmod /lib/modules/5.0.1-gnu/video/nvidia-drm.ko
insmod /lib/modules/5.0.1-gnu/video/nvidia-uvm.ko
