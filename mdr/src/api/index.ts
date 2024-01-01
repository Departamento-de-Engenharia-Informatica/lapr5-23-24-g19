import { Router } from 'express'

import auth from './routes/authRoute'
// import user from './routes/userRoute'
import role from './routes/roleRoute'

import client from './routes/clientRoute'
import backofficeUser from './routes/backofficeUserRoute'
import building from './routes/buildingRoute'
import passage from './routes/passageRoute'
import robot from './routes/robotRoute'
import robotType from './routes/robotTypeRoute'
import task from './routes/taskRoute'
import path from './routes/pathRoute'

export default () => {
    const app = Router()

    auth(app)
    role(app)
    // user(app)

    backofficeUser(app)
    building(app)
    client(app)
    passage(app)
    path(app)
    robot(app)
    robotType(app)
    task(app)

    return app
}
