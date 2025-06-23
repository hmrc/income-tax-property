
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

- Rancher/Docker: Follow the installation guide on HMRC confluence 
- MongoDB: Follow the [MongoDB](https://docs.mongodb.com/manual/installation/) installation guide to install and set up MongoDB being used by HMRC at the time
- Service Manager: Install/configure Service Manager 2 [sm2](https://github.com/hmrc/sm2) to manage and run the service locally.

Start MongoDB (if it isn't already running):

    docker run --restart unless-stopped -d -p 27017-27019:27017-27019 --name mongodb mongo:4.2

Start the Income Tax Property Service: Use the Service Manager to start the service with the following command:

    sm2 --start INCOME_TAX_PROPERTY

This service runs on port: `localhost:19160`

Run the following command to start the additional required services locally:

    sm2 --start INCOME_TAX_SUBMISSION_ALL

To test the branch you're working on locally. You will need to run `sm2 --stop INCOME_TAX_PROPERTY` followed by
`./run.sh`

### Running Tests

- Run Unit Tests:  `sbt test`
- Run Integration Tests: `sbt it/test`
- Run Unit and Integration Tests: `sbt test it/test`
- Run Unit and Integration Tests with coverage report: `./check.sh`<br/>
  which runs `sbt clean coverage test it/test coverageReport dependencyUpdates`

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").