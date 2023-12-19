import config from '../../config'
import { Service, Inject } from 'typedi'
import { Either, left, right } from '../core/logic/Result'
import IRobotTypeService, {
    RobotTypeErrorCode,
    RobotTypeErrorResult,
} from './IServices/IRobotTypeService'
import { IRobotTypeDTO } from '../dto/IRobotTypeDTO'
import IRobotTypeRepo from './IRepos/IRobotTypeRepo'
import { RobotTypeCode } from '../domain/robotType/robotTypeCode'
import { RobotTypeBrand } from '../domain/robotType/robotTypeBrand'
import { RobotTypeModel } from '../domain/robotType/robotTypeModel'
import { TaskType } from '../domain/robotType/taskType'
import RobotType from '../domain/robotType/robotType'
import { RobotTypeMap } from '../mappers/RobotTypeMap'

@Service()
export default class RobotTypeService implements IRobotTypeService {
    constructor(
        @Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo,
    ) {}

    async createRobotType(
        dto: IRobotTypeDTO,
    ): Promise<Either<RobotTypeErrorResult, IRobotTypeDTO>> {
        try {
            const dtoCode = RobotTypeCode.create(dto.code).getOrThrow()
            const dtoBrand = RobotTypeBrand.create(dto.brand).getOrThrow()
            const dtoModel = RobotTypeModel.create(dto.model).getOrThrow()

            let dtoTaskType: TaskType[]
            try {
                dtoTaskType = dto.taskTypes.map((taskType) => TaskType.toType(taskType))
            } catch (e) {
                return left({
                    errorCode: RobotTypeErrorCode.BussinessRuleViolation,
                    message: (e as Error).message,
                })
            }

            const robotType = RobotType.create({
                code: dtoCode,
                brand: dtoBrand,
                model: dtoModel,
                taskType: dtoTaskType,
            }).getOrThrow()

            if (await this.robotTypeRepo.exists(robotType)) {
                return left({
                    errorCode: RobotTypeErrorCode.BussinessRuleViolation,
                    message: 'Robot type already exists',
                })
            }

            const saved = await this.robotTypeRepo.save(robotType)
            return right(RobotTypeMap.toDTO(saved))
        } catch (e) {
            return left({
                errorCode: RobotTypeErrorCode.BussinessRuleViolation,
                message: 'Error creating robot type',
            })
        }
    }

    async getRobotTypes(): Promise<Either<RobotTypeErrorResult, IRobotTypeDTO[]>> {
        try {
            const types = await this.robotTypeRepo.findAll()

            if (types.length === 0) {
                return left({
                    errorCode: RobotTypeErrorCode.NotFound,
                    message: 'Robots not found',
                })
            } else {
                return right(types.map((type) => RobotTypeMap.toDTO(type)))
            }
        } catch (e) {
            return left({
                errorCode: RobotTypeErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }
}
