LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_MODULE := cyber
#LOCAL_SRC_FILES := libs/$(TARGET_ARCH_ABI)/libcyber.a
LOCAL_SRC_FILES := libcyber.a
LOCAL_EXPORT_C_INCLUDES := $(LOCAL_PATH)/include
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE := main.out
LOCAL_SRC_FILES += main.c
LOCAL_STATIC_LIBRARIES := cyber
include $(BUILD_EXECUTABLE)
