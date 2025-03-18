[1mdiff --git a/README.md b/README.md[m
[1mindex f6f0481..7b3bf1c 100644[m
[1m--- a/README.md[m
[1m+++ b/README.md[m
[36m@@ -3,18 +3,22 @@[m
 [m
 ## Running the service locally[m
 [m
[31m-You will need to have the following:[m
[31m-- Installed [MondoDB](https://docs.mongodb.com/manual/installation/)[m
[31m-- Installed/configured [service manager](https://github.com/hmrc/service-manager).[m
[32m+[m[32mTo run the service locally, ensure that the following dependencies are installed and properly configured:[m
 [m
[31m-The service manager profile for this service is:[m
[32m+[m[32m- MongoDB: Follow the [MongoDB](https://docs.mongodb.com/manual/installation/) installation guide to install and set up MongoDB.[m
[32m+[m[32m- Service Manager: Install/configure Service Manager 2 [sm2](https://github.com/hmrc/sm2) to manage and run the service locally.[m
 [m
[31m-    sm --start INCOME_TAX_PROPERTY[m
[31m-[m
[31m-Run the following command to start the remaining services locally:[m
[32m+[m[32mStart MongoDB (if it isn't already running):[m
 [m
     sudo mongod (If not already running)[m
[31m-    sm --start INCOME_TAX_SUBMISSION_ALL -r[m
[32m+[m
[32m+[m[32mStart the Income Tax Property Service: Use the Service Manager to start the service with the following command:[m
[32m+[m
[32m+[m[32m    sm2 --start INCOME_TAX_PROPERTY[m
[32m+[m
[32m+[m[32mRun the following command to start the additional required services locally:[m
[32m+[m
[32m+[m[32m    sm2 --start INCOME_TAX_SUBMISSION_ALL -r[m
 [m
 ### License[m
 [m
