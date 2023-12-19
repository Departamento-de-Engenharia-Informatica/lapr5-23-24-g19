import { Inject, Service } from 'typedi'
import { Model, Document } from 'mongoose'
import config from '../../../config'

import IRobotRepo, { RobotDataMap } from '../../services/IRepos/IRobotRepo'
import MongoRobotDataMap from './dataMapper/RobotDataMapper'
import { IRobotPersistence } from '../../dataschema/mongo/IRobotPersistence'

import Robot from '../../domain/robot/Robot'
import { RobotCode } from '../../domain/robot/code'

@Service()
export default class RobotRepo implements IRobotRepo {
    private readonly mapper: RobotDataMap<IRobotPersistence>

    constructor(
        @Inject(config.schemas.robot.name)
        private schema: Model<IRobotPersistence & Document>,
    ) {
        // this shouldn't be needed; check class impl for more info
        this.mapper = new MongoRobotDataMap()
    }

    async save(t: Robot): Promise<Robot> {
        const doc = await this.schema.findOne({ code: t.code.value })

        try {
            const raw = await this.mapper.toPersistence(t)

            if (!doc) {
                const robot = await this.schema.create(raw)
                return this.mapper.toDomain(robot)
            } else {
                doc.state = raw.state

                // ...

                const saved = await doc.save()
                return this.mapper.toDomain(saved)
            }
        } catch (err) {
            throw err
        }
    }

    async exists(t: Robot): Promise<boolean> {
        return !!(await this.schema.findOne({ code: t.code.value }))
    }

    async find(code: RobotCode): Promise<Robot> {
        const doc = await this.schema.findOne({ code: code.value })
        if (!doc) {
            return null
        }
        return this.mapper.toDomain(doc)
    }

    public async findAll(): Promise<Robot[]> {
        const records = await this.schema.find()

        if (records.length === 0) {
            return [] // Return an empty array when there are no records
        }
        const robotList = await Promise.all(
            records.map((record) => this.mapper.toDomain(record)),
        )
        return robotList
    }
}
