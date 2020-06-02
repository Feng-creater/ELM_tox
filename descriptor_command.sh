# Generating training set and verification set molecular fingerprints
java -jar E:/software/PaDEL/PaDEL-Descriptor.jar -fingerprints -dir data_train -file Train_fp.csv -descriptortypes descriptors.xml
java -jar E:/software/PaDEL/PaDEL-Descriptor.jar -fingerprints -dir data_validate -file Validate_fp.csv -descriptortypes descriptors.xml
