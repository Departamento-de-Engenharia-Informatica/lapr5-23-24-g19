import config from '../../config'
import { Service, Inject } from 'typedi'
import { Result, Either, left, right } from '../core/logic/Result'

import IRobotService, { RobotErrorCode, RobotErrorResult } from './IServices/IRobotService'
import { RobotMap } from '../mappers/RobotMap'
import { IRobotDTO } from '../dto/IRobotDTO'

import IRobotRepo from './IRepos/IRobotRepo'
import IRobotTypeRepo from './IRepos/IRobotTypeRepo'

import { RobotTypeCode } from '../domain/robotType/robotTypeCode'

import Robot from '../domain/robot/Robot'
import { RobotCode } from '../domain/robot/code'
import { RobotNickname as Nickname } from '../domain/robot/nickname'
import { RobotSerialNumber as SerialNumber } from '../domain/robot/serialNumber'
import { RobotDescription as Description } from '../domain/robot/description'
import { ICreatedRobotDTO } from '../dto/ICreatedRobotDTO'
import { IRobotInhibitDTO } from '../dto/IRobotInhibitDTO'

@Service()
export default class RobotService implements IRobotService {
    constructor(
        @Inject(config.repos.robot.name) private robotRepo: IRobotRepo,
        @Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo,
    ) {}

    async createRobot(dto: IRobotDTO): Promise<Either<RobotErrorResult, ICreatedRobotDTO>> {
        try {
            const type = await this.robotTypeRepo.find(RobotTypeCode.create(dto.typeCode).getValue())

            if (!type) {
                return left({
                    errorCode: RobotErrorCode.NotFound,
                    message: `Could not find robot type: ${type}`,
                })
            }

            const code = this.getResult(RobotCode.create(dto.code))
            const nickname = this.getResult(Nickname.create(dto.nickname))
            const serialNumber = this.getResult(SerialNumber.create(dto.serialNumber))
            const description = dto.description && this.getResult(Description.create(dto.description))

            const robot = Robot.create({
                code,
                nickname,
                type,
                serialNumber,
                description,
            })

            if (robot.isFailure) {
                return left({
                    errorCode: RobotErrorCode.BussinessRuleViolation,
                    message: robot.errorValue().toString(),
                })
            }

            const saved = await this.robotRepo.save(robot.getValue())
            return right(RobotMap.toDTO(saved))
        } catch (e) {
            return left({
                errorCode: RobotErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }

    async inhibitRobot(dto: IRobotInhibitDTO): Promise<Either<RobotErrorResult, ICreatedRobotDTO>> {
        const robot = await this.robotRepo.find(RobotCode.create(dto.code).getValue())

        if (!robot) {
            return left({
                errorCode: RobotErrorCode.NotFound,
                message: `Could not find robot type: ${dto.code}`,
            })
        }

        robot.inhibit()
        const saved = await this.robotRepo.save(robot)
        return right(RobotMap.toDTO(saved))
    }

    async getRobots(): Promise<Either<RobotErrorResult, ICreatedRobotDTO[]>> {
        try {
            const robots = await this.robotRepo.findAll()

            if (robots.length === 0) {
                return left({
                    errorCode: RobotErrorCode.NotFound,
                    message: 'Robots not found',
                })
            } else {
                return right(robots.map(robot => RobotMap.toDTO(robot)))
            }
        } catch (e) {
            return left({
                errorCode: RobotErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }

        // TODO: change to this?
        // try {
        //     const robots = await this.robotRepo.findAll()
        //     return right(robots.map(robot => RobotMap.toDTO(robot)))
        // } catch (e) {
        //     throw e
        // }
    }

    // TODO: refactor to Utils
    private getResult<T>(result: Result<T>): T {
        if (result.isFailure) throw new Error(result.errorValue().toString())
        return result.getValue()
    }
}
