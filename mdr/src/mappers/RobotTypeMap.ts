import { Mapper } from '../core/infra/Mapper'

import Room from '../domain/room/room'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'
import { IRobotTypeDTO } from '../dto/IRobotTypeDTO'
import RobotType from '../domain/robotType/robotType'
import { IRobotTypePersistence } from '../dataschema/mongo/IRobotTypePersistence'
import { RobotTypeCode } from '../domain/robotType/robotTypeCode'
import { RobotTypeBrand } from '../domain/robotType/robotTypeBrand'
import { RobotTypeModel } from '../domain/robotType/robotTypeModel'
import { TaskType } from '../domain/robotType/taskType'

export class RobotTypeMap extends Mapper<Room> {
    public static toDTO(robotType: RobotType): IRobotTypeDTO {
        return {
            code: robotType.code.value,
            brand: robotType.brand.value,
            model: robotType.model.value,
            taskTypes: robotType.taskType.map((taskType) => TaskType.toString(taskType)),
        }
    }

    public static async toDomain(raw: IRobotTypePersistence): Promise<RobotType> {
        const rawCode = RobotTypeCode.create(raw.code).getValue()
        const rawBrand = RobotTypeBrand.create(raw.brand).getValue()
        const rawModel = RobotTypeModel.create(raw.model).getValue()
        const rawTaskType = raw.taskType.map((taskType) => TaskType.toType(taskType))

        const robotTypeOrError = RobotType.create(
            {
                code: rawCode,
                brand: rawBrand,
                model: rawModel,
                taskType: rawTaskType,
            },
            new UniqueEntityID(raw.domainId),
        )

        return robotTypeOrError.isSuccess ? robotTypeOrError.getValue() : null
    }

    public static toPersistence(robotType: RobotType): IRobotTypePersistence {
        return {
            domainId: robotType.id.toString(),
            code: robotType.code.value,
            brand: robotType.brand.value,
            model: robotType.model.value,
            taskType: robotType.taskType.map((taskType) => TaskType.toString(taskType)),
        }
    }
}
