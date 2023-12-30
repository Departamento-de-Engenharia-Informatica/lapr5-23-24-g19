import { Inject, Service } from 'typedi'
import { Either, left, right } from '../core/logic/Result'

import { CreateDeliveryTaskDTO } from '../dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from '../dto/CreateSurveillanceTaskDTO'
import config from '../../config'
import { BuildingCode } from '../domain/building/code'
import { FloorNumber } from '../domain/floor/floorNumber'
import { TaskType } from '../domain/robotType/taskType'
import { RoomName } from '../domain/room/roomName'
import { ICreateDeliveryTaskToMapperDTO } from '../dto/ICreateDeliveryTaskToMapperDTO'
import { IFilterDTO } from '../dto/IFilterDTO'
import { IFloorMapInitPositionDTO } from '../dto/IFloorMapInitPositionDTO'
import { ITaskTypeDTO } from '../dto/ITaskTypeDTO'
import { IUpdateTaskDTO } from '../dto/IUpdateTaskDTO'
import { TaskMap } from '../mappers/TaskMap'
import { IStorageFs } from './IFs/IStorageFs'
import IFloorRepo from './IRepos/IFloorRepo'
import IMdtAdapter from './IRepos/IMdtRepo'
import IRobotRepo from './IRepos/IRobotRepo'
import IRoomRepo from './IRepos/IRoomRepo'
import ITaskService, { TaskErrorCode, TaskErrorResult } from './IServices/ITaskService'
import { ITaskAlgorithmDTO } from '../dto/ITaskAlgorithmDTO'
import { IRobotTasksDTO } from '../dto/IRobotTasksDTO'
import { IGeneralTaskDTO } from '../dto/IGeneralTaskDTO'
import { ISequenceAlgorithmDTO } from '../dto/ISequenceAlgorithmDTO'

import { shuffle } from 'lodash'
import { IRobotTaskSequenceDTO } from '../dto/IRobotTaskSequenceDTO'

@Service()
export default class TaskService implements ITaskService {
    constructor(
        @Inject(config.repos.mdt.name) private repo: IMdtAdapter,
        @Inject(config.storage.name) private storage: IStorageFs,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
        @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
        @Inject(config.repos.robot.name) private robotRepo: IRobotRepo,
    ) {}

    async getByFilter(dto: IFilterDTO): Promise<Either<TaskErrorResult, String>> {
        try {
            const saved = await this.repo.getByFilter(dto)

            if (saved === null) {
                console.log('null')
                return left({
                    errorCode: TaskErrorCode.BussinessRuleViolation,
                    message: 'Error fetching tasks',
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

    async getByStatus(
        status: string,
    ): Promise<Either<TaskErrorResult, IGeneralTaskDTO[]>> {
        try {
            const tasks = await this.repo.getByStatus(status)

            if (tasks === null) {
                return left({
                    errorCode: TaskErrorCode.BussinessRuleViolation,
                    message: 'Error fetching tasks',
                })
            }

            return right(tasks.map((t) => TaskMap.toGeneralTaskDto(t)))
        } catch (e) {
            return left({
                errorCode: TaskErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }

    async getTypes(): Promise<Either<TaskErrorResult, ITaskTypeDTO[]>> {
        try {
            const values = Object.values(TaskType).filter(
                (value) => typeof value === 'string',
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
        const startRoom = await this.roomRepo.find(
            BuildingCode.create(dto.startBuildingCode).getValue(),
            FloorNumber.create(dto.startFloorNumber).getValue(),
            RoomName.create(dto.startRoom).getValue(),
        )

        const goalRoom = await this.roomRepo.find(
            BuildingCode.create(dto.goalBuildingCode).getValue(),
            FloorNumber.create(dto.goalFloorNumber).getValue(),
            RoomName.create(dto.goalRoom).getValue(),
        )

        try {
            const saved = await this.repo.createDeliveryTask(
                await TaskMap.deliveryDtoToTaskDto({
                    email: dto.email,

                    startBuildingCode: dto.startBuildingCode,
                    startFloorNumber: dto.startFloorNumber,
                    startRoom: {
                        x: startRoom.positions.x,
                        y: startRoom.positions.y,
                    },

                    goalBuildingCode: dto.goalBuildingCode,
                    goalFloorNumber: dto.goalFloorNumber,
                    goalRoom: {
                        x: goalRoom.positions.x,
                        y: goalRoom.positions.y,
                    },

                    pickupContactName: dto.pickupContactName,
                    pickupContactPhone: dto.pickupContactPhone,

                    deliveryContactName: dto.deliveryContactName,
                    deliveryContactPhone: dto.deliveryContactPhone,

                    description: dto.description,
                    confirmationCode: dto.confirmationCode,
                } as ICreateDeliveryTaskToMapperDTO),
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

    async updateTask(dto: IUpdateTaskDTO): Promise<Either<TaskErrorResult, string>> {
        try {
            const task = await this.repo.updateTask(dto)
            return right(task)
        } catch (e) {
            return left({
                errorCode: TaskErrorCode.BussinessRuleViolation,
                message: e.message ?? e,
            })
        }
    }

    async taskSequence(
        dto: ITaskAlgorithmDTO,
    ): Promise<Either<TaskErrorResult, IRobotTaskSequenceDTO>> {
        try {
            const robots = await this.robotRepo.findAll()

            if (robots.length === 0) {
                return left({
                    errorCode: TaskErrorCode.NotFound,
                    message: 'No robots found',
                })
            }

            const types = new Set(
                dto.tasks.map((t) => TaskType.toType(t.type.toUpperCase())),
            )
            for (const t of types) {
                if (!robots.find((r) => r.type.taskType.includes(t))) {
                    return left({
                        errorCode: TaskErrorCode.NotFound,
                        message: `No robot to fulfill task of type ${TaskType.toString(
                            t,
                        )}`,
                    })
                }
            }

            const tasks = [...dto.tasks]
            const result: IRobotTasksDTO = {
                Algorithm: dto.algorithm,
                RobotTasks: {},
            }

            const shuffledRobots = shuffle(robots)
            while (tasks.length !== 0) {
                shuffledRobots.forEach((r) => {
                    if (
                        tasks.length > 0 &&
                        r.type.taskType.includes(
                            TaskType.toType(tasks[0].type.toUpperCase()),
                        )
                    ) {
                        if (!result.RobotTasks[r.nickname.value]) {
                            result.RobotTasks[r.nickname.value] = []
                        }

                        result.RobotTasks[r.nickname.value].push(tasks.shift())
                    }
                })
            }

            const sequence = await this.repo.taskSequence(result)
            return right(sequence)
        } catch (e) {
            return left({
                errorCode: TaskErrorCode.BussinessRuleViolation,
                message: e.message ?? e,
            })
        }
    }

    async taskSequenceAlgorithms(): Promise<
        Either<TaskErrorResult, ISequenceAlgorithmDTO[]>
    > {
        try {
            const algorithms = await this.repo.getTaskSequenceAlgorithms()

            return right(algorithms)
        } catch (e) {
            return left({
                errorCode: TaskErrorCode.AdapterFailure,
                message: e.message ?? e,
            })
        }
    }
}
