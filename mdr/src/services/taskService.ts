import { Inject, Service } from 'typedi'
import { Either, left, right } from '../core/logic/Result'

import { CreateSurveillanceTaskDTO } from '../../../spa/src/app/dto/CreateSurveillanceTaskDTO'
import config from '../../config'
import { BuildingCode } from '../domain/building/code'
import { FloorNumber } from '../domain/floor/floorNumber'
import { TaskType } from '../domain/robotType/taskType'
import { IFloorMapInitPositionDTO } from '../dto/IFloorMapInitPositionDTO'
import { ITaskTypeDTO } from '../dto/ITaskTypeDTO'
import HttpNodeMdtAdapter from '../repos/mdt/httpNodeMdtAdapter'
import { IStorageFs } from './IFs/IStorageFs'
import IFloorRepo from './IRepos/IFloorRepo'
import ITaskService, { TaskErrorCode, TaskErrorResult } from './IServices/ITaskService'
import { TaskMap } from '../mappers/TaskMap'
import { CreateDeliveryTaskDTO } from '../../../spa/src/app/dto/CreateDeliveryTaskDTO'

@Service()
export default class TaskService implements ITaskService {
    constructor(
        @Inject(config.repos.mdt.name) private repo: HttpNodeMdtAdapter,
        @Inject(config.storage.name) private storage: IStorageFs,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    ) {}

    async getTypes(): Promise<Either<TaskErrorResult, ITaskTypeDTO[]>> {
        try {
            const values = Object.values(TaskType).filter(
                value => typeof value === 'string',
            )

            const res = values.map((type: TaskType) => ({
                description: TaskType.toString(type),
            })) as ITaskTypeDTO[]

            return right(res)
        } catch (e) {
            return left({
                errorCode: TaskErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }

    async createSurveillanceTask(
        dto: CreateSurveillanceTaskDTO,
    ): Promise<Either<TaskErrorResult, String>> {
        console.log(dto)

        const floor = await this.floorRepo.findByCodeNumber(
            BuildingCode.create(dto.buildingCode).getOrThrow(),
            FloorNumber.create(dto.floorNumber).getValue(),
        )

        if (floor === null) {
            return left({
                errorCode: TaskErrorCode.NotFound,
                message: 'Floor not found',
            })
        } else if (!floor.mapPath) {
            return left({
                errorCode: TaskErrorCode.NotFound,
                message: 'Floor does not contain a map',
            })
        }

        const map = await this.storage.get<IFloorMapInitPositionDTO>(floor.mapPath)

        const mapX = map.player.initialPosition[0]
        const mapY = map.player.initialPosition[1]

        try {
            const saved = await this.repo.createSurveillanceTask(
                TaskMap.surveillanceDtoToTaskDto(dto, mapX, mapY),
            )

            if (saved === null) {
                return left({
                    errorCode: TaskErrorCode.BussinessRuleViolation,
                    message: 'Task not created',
                })
            }

            return right(saved)
        } catch (e) {
            return left({
                errorCode: TaskErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }

    async createDeliveryTask(
        dto: CreateDeliveryTaskDTO,
    ): Promise<Either<TaskErrorResult, String>> {
        console.log(dto)

        try {
            const saved = await this.repo.createDeliveryTask(
                await TaskMap.deliveryDtoToTaskDto(dto),
            )

            if (saved === null) {
                return left({
                    errorCode: TaskErrorCode.BussinessRuleViolation,
                    message: 'Task not created',
                })
            }

            return right(saved)
        } catch (e) {
            return left({
                errorCode: TaskErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }
}
