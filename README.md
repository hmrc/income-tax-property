
# income-tax-property

## Running the service locally

To run the service locally, ensure that the following dependencies are installed and properly configured:

- Rancher/Docker: Follow the installation guide on HMRC confluence 
- MongoDB: Follow the [MongoDB](https://docs.mongodb.com/manual/installation/) installation guide to install and set up MongoDB being used by HMRC at the time
- Service Manager: Install/configure Service Manager 2 [sm2](https://github.com/hmrc/sm2) to manage and run the service locally.

Start MongoDB (if it isn't already running):

    docker run --restart unless-stopped -d -p 27017-27019:27017-27019 --name mongodb mongo:4.2

Start the Income Tax Property Service: Use the Service Manager to start the service with the following command:

    sm2 --start INCOME_TAX_PROPERTY

Run the following command to start the additional required services locally:

    sm2 --start INCOME_TAX_SUBMISSION_ALL -r

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").