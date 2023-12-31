import { shuffle } from 'lodash'
import { Service } from 'typedi'
import Robot from '../../../domain/robot/Robot'
import { TaskType } from '../../../domain/robotType/taskType'
import { ITaskIdsDTO } from '../../../dto/ITaskIdsDTO'
import { Result } from '../Result'
import ITaskDistributionStrategy, { AssignedTasks } from './ITaskDistributionStrategy'

@Service()
export default class RoundRobinDistribution implements ITaskDistributionStrategy {
    constructor() {}

    distribute(tasks: ITaskIdsDTO[], robots: Robot[]): Result<AssignedTasks> {
        const fulfill = this.canFulfill(tasks, robots)

        if (fulfill.isFailure) {
            return Result.fail(fulfill.errorValue())
        }

        const result: AssignedTasks = {}

        const shuffledRobots = shuffle(robots)
        while (tasks.length !== 0) {
            shuffledRobots.forEach((r) => {
                if (
                    tasks.length > 0 &&
                    r.type.taskType.includes(TaskType.toType(tasks[0].type.toUpperCase()))
                ) {
                    if (!result[r.nickname.value]) {
                        result[r.nickname.value] = []
                    }

                    result[r.nickname.value].push(tasks.shift())
                }
            })
        }

        return Result.ok(result)
    }

    private canFulfill(tasks: ITaskIdsDTO[], robots: Robot[]): Result<string> {
        const types = new Set(tasks.map((t) => TaskType.toType(t.type.toUpperCase())))
        for (const t of types) {
            if (!robots.find((r) => r.type.taskType.includes(t))) {
                return Result.fail(
                    `No robot to fulfill task of type ${TaskType.toString(t)}`,
                )
            }
        }
        return Result.ok()
    }
}
