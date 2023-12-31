import 'reflect-metadata'

import { expect } from 'chai'
import { describe, it } from 'mocha'
import { createSandbox } from 'sinon'
import Container from 'typedi'
import { Result } from '../core/logic/Result'
import config from '../../config'
import IMdtAdapter from './IRepos/IMdtRepo'
import { IStorageFs } from './IFs/IStorageFs'
import IFloorRepo from './IRepos/IFloorRepo'
import IRoomRepo from './IRepos/IRoomRepo'
import IRobotRepo from './IRepos/IRobotRepo'
import ITaskDistributionStrategy from '../core/logic/taskDistribution/ITaskDistributionStrategy'
import TaskService from './taskService'
import { ITaskAlgorithmDTO } from '../dto/ITaskAlgorithmDTO'
import { TaskErrorCode, TaskErrorResult } from './IServices/ITaskService'
import Robot from '../domain/robot/Robot'
import { IRobotTaskSequenceDTO } from '../dto/IRobotTaskSequenceDTO'
import { ISequenceAlgorithmDTO } from '../dto/ISequenceAlgorithmDTO'

describe('Client Service: Unit tests', () => {
    const sinon = createSandbox()

    // function stubCreate<K>(klass: K) {
    //     sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    // }

    beforeEach(() => {
        Container.reset()

        const mdtAdapterClass = require('../repos/mdt/httpNodeMdtAdapter').default
        const mdtAdapter = Container.get(mdtAdapterClass)
        Container.set(config.repos.mdt.name, mdtAdapter)

        const storageFsClass = require('../repos/mdt/httpNodeMdtAdapter').default
        const storageFs = Container.get(storageFsClass)
        Container.set(config.storage.name, storageFs)

        let buildingSchema = require('../persistence/schemas/buildingSchema').default
        Container.set('buildingSchema', buildingSchema)

        let floorSchema = require('../persistence/schemas/floorSchema').default
        Container.set('floorSchema', floorSchema)

        let floorRepoClass = require('../repos/mongo/floorRepo').default
        let floorRepo = Container.get(floorRepoClass)
        Container.set('FloorRepo', floorRepo)

        let roomSchema = require('../persistence/schemas/roomSchema').default
        Container.set('roomSchema', roomSchema)

        let roomRepoClass = require('../repos/mongo/roomRepo').default
        let roomRepo = Container.get(roomRepoClass)
        Container.set('RoomRepo', roomRepo)

        let robotTypeSchema = require('../persistence/schemas/robotTypeSchema').default
        Container.set('robotType', robotTypeSchema)

        let robotTypeRepoClass = require('../repos/mongo/robotTypeRepo').default
        let robotTypeRepo = Container.get(robotTypeRepoClass)
        Container.set('RobotTypeRepo', robotTypeRepo)

        let robotSchema = require('../persistence/schemas/robotSchema').default
        Container.set('robotSchema', robotSchema)

        let robotRepoClass = require('../repos/mongo/robotRepo').default
        let robotRepo = Container.get(robotRepoClass)
        Container.set('RobotRepo', robotRepo)

        let distributionStrategyClass =
            require('../core/logic/taskDistribution/roundRobinDistribution').default
        let distributionStrategy = Container.get(distributionStrategyClass)
        Container.set(config.strategies.taskDistribution.name, distributionStrategy)

        // stubCreate()
    })

    afterEach(() => sinon.restore())

    describe('taskSequence()', () => {
        it('should return TaskErrorResult when no robots are found', async () => {
            const mdt = Container.get(config.repos.mdt.name) as IMdtAdapter
            const storage = Container.get(config.storage.name) as IStorageFs
            const floors = Container.get(config.repos.floor.name) as IFloorRepo
            const rooms = Container.get(config.repos.room.name) as IRoomRepo
            const robots = Container.get(config.repos.robot.name) as IRobotRepo
            const strategy = Container.get(
                config.strategies.taskDistribution.name,
            ) as ITaskDistributionStrategy

            const dto: ITaskAlgorithmDTO = {
                algorithm: 'permutations',
                tasks: [{ id: 'task1', type: 'SURVEILLANCE' }],
            }

            sinon.stub(robots, 'findAll').resolves([])

            const svc = new TaskService(mdt, storage, floors, rooms, robots, strategy)
            const result = await svc.taskSequence(dto)

            expect(result.isLeft()).to.be.true

            const err = result.value as TaskErrorResult
            expect(err.errorCode).to.equal(TaskErrorCode.NotFound)
            // expect(err.message).to.equal('No robots found');
        })

        it('should return TaskErrorResult when task distribution fails', async () => {
            const mdt = Container.get(config.repos.mdt.name) as IMdtAdapter
            const storage = Container.get(config.storage.name) as IStorageFs
            const floors = Container.get(config.repos.floor.name) as IFloorRepo
            const rooms = Container.get(config.repos.room.name) as IRoomRepo
            const robots = Container.get(config.repos.robot.name) as IRobotRepo
            const strategy = Container.get(
                config.strategies.taskDistribution.name,
            ) as ITaskDistributionStrategy

            const dto: ITaskAlgorithmDTO = {
                algorithm: 'permutations',
                tasks: [{ id: 'task1', type: 'SURVEILLANCE' }],
            }

            sinon.stub(robots, 'findAll').resolves([
                {
                    type: { taskType: ['DELIVERY'] },
                } as Robot,
            ])

            const errMsg = 'Error'
            sinon.stub(strategy, 'distribute').returns(Result.fail(errMsg))

            const svc = new TaskService(mdt, storage, floors, rooms, robots, strategy)
            const result = await svc.taskSequence(dto)

            expect(result.isLeft()).to.be.true

            const err = result.value as TaskErrorResult
            expect(err.errorCode).to.equal(TaskErrorCode.BussinessRuleViolation)
            expect(err.message).to.equal(errMsg)
        })

        it('should return IRobotTaskSequenceDTO when tasks are successfully distributed', async () => {
            const mdt = Container.get(config.repos.mdt.name) as IMdtAdapter
            const storage = Container.get(config.storage.name) as IStorageFs
            const floors = Container.get(config.repos.floor.name) as IFloorRepo
            const rooms = Container.get(config.repos.room.name) as IRoomRepo
            const robots = Container.get(config.repos.robot.name) as IRobotRepo
            const strategy = Container.get(
                config.strategies.taskDistribution.name,
            ) as ITaskDistributionStrategy

            const dto: ITaskAlgorithmDTO = {
                algorithm: 'permutations',
                tasks: [
                    { id: 'task1', type: 'SURVEILLANCE' },
                    { id: 'task2', type: 'SURVEILLANCE' },
                    { id: 'task3', type: 'DELIVERY' },
                ],
            }

            sinon.stub(robots, 'findAll').resolves([
                {
                    nickname: 'alfredo',
                    type: { taskType: ['SURVEILLANCE'] },
                } as unknown as Robot,
                {
                    nickname: 'maria',
                    type: { taskType: ['DELIVERY'] },
                } as unknown as Robot,
            ])

            sinon.stub(strategy, 'distribute').returns(
                Result.ok({
                    ['alfredo']: [
                        { id: 'task1', type: 'SURVEILLANCE' },
                        { id: 'task2', type: 'SURVEILLANCE' },
                    ],
                    ['maria']: [{ id: 'task3', type: 'SURVEILLANCE' }],
                }),
            )

            const mdtReturnData = [
                // {{{ mdt return data
                {
                    robotName: 'alfredo',
                    tasks: {
                        cost: 10,
                        initialPosition: {
                            building: 'C',
                            floor: 2,
                            x: 14,
                            y: 7,
                        },
                        order: [
                            {
                                start: {
                                    building: 'C',
                                    floor: 2,
                                    x: 12,
                                    y: 2,
                                },
                                end: {
                                    building: 'C',
                                    floor: 2,
                                    x: 12,
                                    y: 12,
                                },
                                taskId: 'task2',
                            },
                            {
                                start: {
                                    building: 'B',
                                    floor: 1,
                                    x: 3,
                                    y: 11,
                                },
                                end: {
                                    building: 'B',
                                    floor: 1,
                                    x: 3,
                                    y: 11,
                                },
                                taskId: 'task1',
                            },
                        ],
                    },
                },
                {
                    robotName: 'maria',
                    tasks: {
                        cost: 147.123,
                        initialPosition: {
                            building: 'D',
                            floor: 3,
                            x: 1,
                            y: 17,
                        },
                        order: [
                            {
                                start: {
                                    building: 'C',
                                    floor: 1,
                                    x: 20,
                                    y: 4,
                                },
                                end: {
                                    building: 'D',
                                    floor: 3,
                                    x: 19,
                                    y: 5,
                                },
                                taskId: 'task3',
                            },
                            ,
                        ],
                    },
                },
                // }}}
            ]

            sinon.stub(mdt, 'taskSequence').resolves(mdtReturnData)

            const svc = new TaskService(mdt, storage, floors, rooms, robots, strategy)
            const result = await svc.taskSequence(dto)

            expect(result.isRight()).to.be.true
            const res = result.value as IRobotTaskSequenceDTO
            expect(res).to.be.an('array')

            expect(res).to.have.lengthOf(mdtReturnData.length)
            expect(res).to.deep.equals(mdtReturnData)
        })
    })

    describe('taskSequenceAlgorithms()', () => {
        it('should retrieve task sequencing algorithms', async () => {
            const mdt = Container.get(config.repos.mdt.name) as IMdtAdapter
            const storage = Container.get(config.storage.name) as IStorageFs
            const floors = Container.get(config.repos.floor.name) as IFloorRepo
            const rooms = Container.get(config.repos.room.name) as IRoomRepo
            const robots = Container.get(config.repos.robot.name) as IRobotRepo
            const strategy = Container.get(
                config.strategies.taskDistribution.name,
            ) as ITaskDistributionStrategy

            const algorithms = ['permutations', 'genetic']

            sinon.stub(mdt, 'getTaskSequenceAlgorithms').resolves(algorithms)

            const svc = new TaskService(mdt, storage, floors, rooms, robots, strategy)

            const result = await svc.taskSequenceAlgorithms()
            expect(result.isRight()).to.be.true
            const res = result.value as ISequenceAlgorithmDTO[]
            expect(res).to.be.an('array')

            expect(res).to.have.lengthOf(algorithms.length)
            expect(res).to.deep.equals(algorithms)
        }),
            it('should error on adapter failure', async () => {
                const mdt = Container.get(config.repos.mdt.name) as IMdtAdapter
                const storage = Container.get(config.storage.name) as IStorageFs
                const floors = Container.get(config.repos.floor.name) as IFloorRepo
                const rooms = Container.get(config.repos.room.name) as IRoomRepo
                const robots = Container.get(config.repos.robot.name) as IRobotRepo
                const strategy = Container.get(
                    config.strategies.taskDistribution.name,
                ) as ITaskDistributionStrategy

                const errMsg = 'Failure in HTTP Request: Bad Request'
                sinon.stub(mdt, 'getTaskSequenceAlgorithms').rejects({ message: errMsg })

                const svc = new TaskService(mdt, storage, floors, rooms, robots, strategy)

                const result = await svc.taskSequenceAlgorithms()
                expect(result.isLeft()).to.be.true

                const res = result.value as TaskErrorResult

                expect(res.errorCode).to.equal(TaskErrorCode.AdapterFailure)
                expect(res.message).to.equal(errMsg)
            })
    })
})
