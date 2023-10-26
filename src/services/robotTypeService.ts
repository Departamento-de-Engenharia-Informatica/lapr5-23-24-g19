import config from '../../config'
import { Service, Inject } from 'typedi'
import { Result } from '../core/logic/Result'
import IRobotTypeService from './IServices/IRobotTypeService'
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
    constructor(@Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo) {}

    public async createRobotType(dto: IRobotTypeDTO): Promise<Result<IRobotTypeDTO>> {
        try {
            const dtoCode = RobotTypeCode.create(dto.code).getValue()
            const dtoBrand = RobotTypeBrand.create(dto.brand).getValue()
            const dtoModel = RobotTypeModel.create(dto.model).getValue()
            const dtoTaskType = TaskType.create({ value: dto.taskType }).getValue()

            const robotType = RobotType.create({
                code: dtoCode,
                brand: dtoBrand,
                model: dtoModel,
                taskType: dtoTaskType,
            }).getValue()

            if (await this.robotTypeRepo.exists(robotType)) {
                return Result.fail(422)
            }
            const saved = await this.robotTypeRepo.save(robotType)
            return Result.ok<IRobotTypeDTO>(RobotTypeMap.toDTO(saved))
        } catch (e) {
            return Result.fail(422)
        }
    }
}
