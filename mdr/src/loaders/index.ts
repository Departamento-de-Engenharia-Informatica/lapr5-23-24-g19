import expressLoader from './express'
import dependencyInjectorLoader from './dependencyInjector'
import mongooseLoader from './mongoose'
import Logger from './logger'

import config from '../../config'

export default async ({ expressApp }) => {
    const mongoConnection = await mongooseLoader()
    Logger.info('✌️ DB loaded and connected!')

    const loaderProps: any = { mongoConnection }
    const keys = ['controllers', 'repos', 'services', 'schemas']
    for (let key of keys) {
        if (config[key] !== null && config[key] !== undefined) {
            loaderProps[key] = Object.values(config[key])
        }
    }


    await dependencyInjectorLoader(loaderProps)
    Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded')

    await expressLoader({ app: expressApp })
    Logger.info('✌️ Express loaded')
}
