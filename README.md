
# income-tax-property

## APIs

APIs consumed by this service:

| API | Description                      | Tax Year   |
|-----|----------------------------------|------------|
| 1171 | Business Details (GET)           | all        |
| 1500 | Brought Forward Losses (CREATE)  | all        |
| 1501 | Brought Forward Losses (UPDATE)  | all        |
| 1502 | Brought Forward Losses (GET)     | all        |
| 1593 | Property Periodic (CREATE)       | pre 23-24  |
| 1594 | Property Periodic (UPDATE)       | pre 23-24  |
| 1595 | Property Periodic (GET)          | pre 23-24  |
| 1596 | Foreign Property Annual (DELETE) | pre 23-24  |
| 1597 | Property Annual (CREATE/UPDATE)  | pre 23-24  |
| 1598 | Property Annual (GET)            | pre 23-24  |
| 1608 | Dividends Income (CREATE/UPDATE) | pre 23-24  |
| 1609 | Dividends Income (GET)           | pre 23-24  |
| 1610 | Dividends Income (DELETE)        | pre 23-24  |
| 1649 | Property Periodic (GET)          | pre 23-24  |
| 1804 | Property Annual (CREATE/UPDATE)  | post 23-24 |
| 1805 | Property Annual (GET)            | post 23-24 |
| 1861 | Property Periodic (CREATE)       | post 23-24 |
| 1862 | Property Periodic (GET)          | post 23-24 |
| 1863 | Foreign Property Annual (DELETE) | post 23-24 |
| 1870 | Brought Forward Losses (LIST)    | post 23-24 |
| 1906 | Dividends Income (CREATE/UPDATE) | post 23-24 |
| 1907 | Dividends Income (GET)           | post 23-24 |
| 1908 | Dividends Income (DELETE)        | post 23-24 |
| 1954 | Property Periodic (LIST)         | post 23-24 |
| 1958 | Property Periodic (UPDATE)       | post 23-24 |

## Running the service locally

To run the service locally, ensure that the following dependencies are installed and properly configured:

- Docker: Follow the [developer set-up guide](https://docs.tax.service.gov.uk/mdtp-handbook/documentation/developer-set-up/)
- MongoDB: Follow the [MongoDB](https://docs.mongodb.com/manual/installation/) installation guide to install and set up MongoDB being used by HMRC at the time
- Service Manager: Install/configure Service Manager 2 [sm2](https://github.com/hmrc/sm2) to manage and run the service locally.

Start MongoDB (if it isn't already running):

    docker run --restart unless-stopped -d -p 27017-27019:27017-27019 --name mongodb mongo:4.2

Start the Income Tax Property Service: Use the Service Manager to start the service with the following command:

    sm2 --start INCOME_TAX_PROPERTY

This service runs on port: `localhost:19160`

Run the following command to start the additional required services locally:

    sm2 --start INCOME_TAX_SUBMISSION_ALL

## Using the service

There are two main flows:

* Agent sign up
* Individual sign up

### Local

#### Individual
* Login via: [http://localhost:9949/auth-login-stub/gg-sign-in](http://localhost:9949/auth-login-stub/gg-sign-in)
* Entry page: [http://localhost:9302/update-and-submit-income-tax-return/2025/start](http://localhost:9302/update-and-submit-income-tax-return/2025/start)

| Enrolment Key | Identifier Name | Identifier Value |
|---------------|-----------------|------------------|
| HMRC-MTD-IT   | MTDITID         | 1234567890       |


### Agent
* Login via: [http://localhost:9949/auth-login-stub/gg-sign-in](http://localhost:9949/auth-login-stub/gg-sign-in)
* Entry page : [http://localhost:9302/update-and-submit-income-tax-return/test-only/2024/additional-parameters?ClientNino=AC180000A&ClientMTDID=1234567890](http://localhost:9302/update-and-submit-income-tax-return/test-only/2024/additional-parameters?ClientNino=AC180000A&ClientMTDID=1234567890)

| Enrolment Key  | Identifier Name      | Identifier Value	 |
|----------------|----------------------|-------------------|
| HMRC-MTD-IT    | MTDITID              | 1234567890        |
| HMRC-AS-AGENT  | AgentReferenceNumber | XARN1234567       |

### Staging

*Requires HMRC VPN*

#### Individual
* Login via: [https://www.staging.tax.service.gov.uk/auth-login-stub/gg-sign-in](https://www.staging.tax.service.gov.uk/auth-login-stub/gg-sign-in)
* Entry page : [http://localhost:9302/update-and-submit-income-tax-return/test-only/2024/additional-parameters?ClientNino=AC180000A&ClientMTDID=1234567890](http://localhost:9302/update-and-submit-income-tax-return/test-only/2024/additional-parameters?ClientNino=AC180000A&ClientMTDID=1234567890)

| Enrolment Key | Identifier Name | Identifier Value |
|---------------|-----------------|------------------|
| HMRC-MTD-IT   | MTDITID         | 1234567890       |

#### Agent
* Login via: [https://www.staging.tax.service.gov.uk/auth-login-stub/gg-sign-in](https://www.staging.tax.service.gov.uk/auth-login-stub/gg-sign-in)
* Entry page : [http://localhost:9302/update-and-submit-income-tax-return/test-only/2024/additional-parameters?ClientNino=AC180000A&ClientMTDID=1234567890](http://localhost:9302/update-and-submit-income-tax-return/test-only/2024/additional-parameters?ClientNino=AC180000A&ClientMTDID=1234567890)

| Enrolment Key  | Identifier Name      | Identifier Value	 |
|----------------|----------------------|-------------------|
| HMRC-MTD-IT    | MTDITID              | 1234567890        |
| HMRC-AS-AGENT  | AgentReferenceNumber | XARN1234567       |


## Testing the service

* Run unit tests: `sbt clean test`
* Run integration tests: `sbt clean it/test`
* Run performance tests: provided in the repo [income-tax-submission-performance-tests](https://github.com/hmrc/income-tax-submission-performance-tests)
* Run acceptance tests: provided in the repo [income-tax-submission-journey-tests](https://github.com/hmrc/income-tax-submission-journey-tests)
* Run Unit and Integration Tests with coverage report and dependency updates: `./check.sh`<br/>
  which runs `sbt clean coverage test it/test coverageReport dependencyUpdates`

## Ninos with stub data for Property in Local/Staging Environment

| Nino      | Description           |
|-----------|-----------------------|
| AC210000B | Traditional (accrual) |
| AC210000A | Cash                  |
| AC180000A | Traditional (accrual) | 
| AC190000B | Cash                  |

### Feature Switches
| Feature                     | Description                                                                                              |
 |-----------------------------|----------------------------------------------------------------------------------------------------------|
| enableHipApis          | Enables a toggle to use the APIs on the Hybrid Integration Platform


### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").