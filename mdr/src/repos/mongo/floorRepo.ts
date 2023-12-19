import { Service, Inject } from 'typedi'
import { Document, Model, PipelineStage } from 'mongoose'
import { IFloorPersistence } from '../../dataschema/mongo/IFloorPersistence'
import IFloorRepo, { BuildingFloorCount } from '../../services/IRepos/IFloorRepo'
import { Floor } from '../../domain/floor/floor'
import { FloorMap } from '../../mappers/FloorMap'
import { FloorNumber } from '../../domain/floor/floorNumber'
import Building from '../../domain/building/building'
import { IBuildingPersistence } from '../../dataschema/mongo/IBuildingPersistence'
import { BuildingCode } from '../../domain/building/code'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'
import { json, raw } from 'body-parser'

@Service()
export default class FloorRepo implements IFloorRepo {
    private models: any

    constructor(
        @Inject('floorSchema') private floorSchema: Model<IFloorPersistence & Document>,
        @Inject('buildingSchema')
        private buildingSchema: Model<IBuildingPersistence & Document>,
    ) {}

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(floor: Floor | string): Promise<boolean> {
        if (floor instanceof Floor) {
            const query = {
                buildingCode: floor.building.code.value,
                floorNumber: floor.floorNumber.value,
            }
            const floorDocument = await this.floorSchema.findOne(query)
            return !!floorDocument === true
        } else {
            return false
        }
    }

    public async findByCodeNumber(
        buildingCode: BuildingCode,
        floorNumber: FloorNumber,
    ): Promise<Floor> {
        const query = {
            buildingCode: buildingCode.value,
            floorNumber: floorNumber.value,
        }

        const floorDocument = await this.floorSchema.findOne(query)

        if (floorDocument != null) {
            return FloorMap.toDomain(floorDocument)
        }

        return null
    }

    public async findByID(floorID: string): Promise<Floor> {
        const query = { domainId: floorID }
        const floorDocument = await this.floorSchema.findOne(query)

        if (floorDocument != null) {
            return FloorMap.toDomain(floorDocument)
        } else return null
    }

    public async findBuildingsByMinMaxFloors(
        min: number,
        max: number,
    ): Promise<BuildingFloorCount[]> {
        const aggregationPipeline = [
            {
                $group: {
                    _id: '$buildingCode',
                    floorCount: { $sum: 1 }, // count the number of floors for each building
                },
            },
            {
                $match: {
                    floorCount: {
                        $gte: min,
                        $lte: max,
                    },
                },
            },
        ]

        return (
            await this.floorSchema.aggregate(aggregationPipeline as PipelineStage[])
        ).map((result) => {
            return {
                buildingCode: BuildingCode.create(result._id).getValue(),
                floorCount: result.floorCount,
            }
        })
    }

    public async findByBuildingCode(code: BuildingCode): Promise<Floor[]> {
        const query = {
            buildingCode: code.value,
        }

        const records = await this.floorSchema.find(query)

        if (records.length == 0) {
            return []
        }

        const floorList = await Promise.all(
            records.map((record) => FloorMap.toDomain(record)),
        )
        return floorList
    }

    public async save(floor: Floor): Promise<Floor> {
        const query = { domainId: floor.id }
        const floorDocument = await this.floorSchema.findOne(query)
        try {
            const rawFloor: any = FloorMap.toPersistence(floor)
            if (floorDocument === null) {
                const floorCreated = await this.floorSchema.create(rawFloor)
                return FloorMap.toDomain(floorCreated)
            } else {
                floorDocument.floorNumber = rawFloor.floorNumber
                floorDocument.buildingCode = rawFloor.buildingCode
                floorDocument.description = rawFloor.description
                floorDocument.path = rawFloor.path
                // floorDocument.map = rawFloor.map

                await floorDocument.save()
                return FloorMap.toDomain(floorDocument)
            }
        } catch (err) {
            throw err
        }
    }

    async find(building: Building, floorNumber: FloorNumber): Promise<Floor> {
        const query = {
            buildingCode: building.code.value,
            floorNumber: floorNumber.value,
        }

        const floor = await this.floorSchema.findOne(query)
        return FloorMap.toDomain(floor)
    }

    public async findAllInBuilding(building: Building): Promise<Floor[]> {
        const query = {
            buildingCode: building.code.value,
        }

        const records = await this.floorSchema.find(query)

        if (records.length === 0) {
            return []
        }
        const passageList = await Promise.all(
            records.map((record) => FloorMap.toDomain(record)),
        )

        return passageList
    }

    async findById(id: string | UniqueEntityID): Promise<Floor> {
        if (id instanceof UniqueEntityID) {
            id = id.toString()
        }

        const floor = await this.floorSchema.findOne({ domainId: id })
        return FloorMap.toDomain(floor)
    }
}
