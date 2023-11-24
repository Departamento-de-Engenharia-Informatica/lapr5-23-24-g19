import { Router } from 'express'
import auth from './routes/userRoute'
import user from './routes/userRoute'
import role from './routes/roleRoute'
import building from './routes/buildingRoute'
import passage from './routes/passageRoute'
import robot from './routes/robotRoute'
import robotType from './routes/robotTypeRoute'
import task from './routes/taskRoute'

export default () => {
    const app = Router()

    auth(app)
    user(app)
    role(app)
    building(app)
    passage(app)
    robot(app)
    task(app)
    robotType(app)

    return app
}

