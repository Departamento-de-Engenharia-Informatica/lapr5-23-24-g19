import config from '../../../config'
import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'

import { IRobotTypePersistence } from '../../dataschema/mongo/IRobotTypePersistence'
import RobotType from '../../domain/robotType/robotType'
import { RobotTypeMap } from '../../mappers/RobotTypeMap'
import IRobotTypeRepo from '../../services/IRepos/IRobotTypeRepo'
import { RobotTypeCode } from '../../domain/robotType/robotTypeCode'

@Service()
export default class RobotTypeRepo implements IRobotTypeRepo {
    constructor(
        @Inject(config.schemas.robotType.name)
        private robotTypeSchema: Model<IRobotTypePersistence & Document>,
    ) {}

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(robotType: RobotType): Promise<boolean> {
        const query = { code: robotType.code.value }
        const robotTypeDocument = await this.robotTypeSchema.findOne(query)
        return !!robotTypeDocument
    }

    public async find(code: RobotTypeCode): Promise<RobotType> {
        const doc = await this.robotTypeSchema.findOne({ code: code.value })
        if (!doc) {
            return null
        }
        return RobotTypeMap.toDomain(doc)
    }

    public async save(robotType: RobotType): Promise<RobotType> {
        const query = { code: robotType.code.value }

        const robotTypeDocument = await this.robotTypeSchema.findOne(query)

        try {
            const rawRobotType = RobotTypeMap.toPersistence(robotType)

            if (!!robotTypeDocument) {
                robotTypeDocument.code = rawRobotType.code
                robotTypeDocument.brand = rawRobotType.brand
                robotTypeDocument.model = rawRobotType.model
                robotTypeDocument.taskType = rawRobotType.taskType

                await robotTypeDocument.save()
                return RobotTypeMap.toDomain(robotTypeDocument)
            }

            const robotTypeCreated = await this.robotTypeSchema.create(rawRobotType)

            return RobotTypeMap.toDomain(robotTypeCreated)
        } catch (err) {
            throw err
        }
    }

    public async findAll(): Promise<RobotType[]> {
        const records = await this.robotTypeSchema.find()

        if (records.length === 0) {
            return [] // Return an empty array when there are no records
        }
        const robotTypeList = await Promise.all(
            records.map((record) => RobotTypeMap.toDomain(record)),
        )
        return robotTypeList
    }
}
