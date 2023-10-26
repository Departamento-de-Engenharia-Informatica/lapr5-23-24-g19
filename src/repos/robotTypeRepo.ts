import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import { IRobotTypePersistence } from '../dataschema/IRobotTypePersistence'
import RobotType from '../domain/robotType/robotType'
import { RobotTypeMap } from '../mappers/RobotTypeMap'
import IRobotTypeRepo from '../services/IRepos/IRobotTypeRepo'
import config from "../../config";

@Service()
export default class RobotTypeRepo implements IRobotTypeRepo {
    constructor(@Inject(config.schemas.robotType.name) private robotTypeSchema: Model<IRobotTypePersistence & Document>) {}

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(robotType: RobotType): Promise<boolean> {
        const query = { code: robotType.code }
        const buildingDocument = await this.robotTypeSchema.findOne(query)

        return !!buildingDocument
    }

    public async save(robotType: RobotType): Promise<RobotType> {
        const query = { code: robotType.code.toString() }

        const robotTypeDocument = await this.robotTypeSchema.findOne(query)

        try {
            const rawRobotType = RobotTypeMap.toPersistence(robotType)

            if (await this.exists(robotType)) {
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
}
