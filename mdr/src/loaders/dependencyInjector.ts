import { Container } from 'typedi'
import LoggerInstance from './logger'

export default ({
    mongoConnection,
    schemas,
    controllers,
    repos,
    services,
    storage,
}: {
    mongoConnection
    schemas: { name: string; schema: any }[]
    controllers: { name: string; path: string }[]
    repos: { name: string; path: string }[]
    services: { name: string; path: string }[]
    storage: { name: string; path: string; prefix: string }
}) => {
    try {
        Container.set('logger', LoggerInstance)

        // set up file storage
        let storageClass = require(storage.path).default
        let storageInstance = Container.get(storageClass)
        Container.set(storage.name, storageInstance)

        /**
         * We are injecting the mongoose models into the DI container.
         * This is controversial but it will provide a lot of flexibility
         * at the time of writing unit tests.
         */
        schemas.forEach((m) => {
            // Notice the require syntax and the '.default'
            let schema = require(m.schema).default
            Container.set(m.name, schema)
        })

        repos.forEach((m) => {
            let repoClass = require(m.path).default
            let repoInstance = Container.get(repoClass)
            Container.set(m.name, repoInstance)
        })

        services.forEach((m) => {
            let serviceClass = require(m.path).default
            let serviceInstance = Container.get(serviceClass)
            Container.set(m.name, serviceInstance)
        })

        controllers.forEach((m) => {
            // load the @Service() class by its path
            let controllerClass = require(m.path).default
            // create/get the instance of the @Service() class
            let controllerInstance = Container.get(controllerClass)
            // rename the instance inside the container
            Container.set(m.name, controllerInstance)
        })

        return
    } catch (e) {
        LoggerInstance.error('🔥 Error on dependency injector loader: %o', e)
        throw e
    }
}
