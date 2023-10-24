import { Service, Inject } from 'typedi'
import { Document, Model, PipelineStage } from 'mongoose'
import { IFloorPersistence } from '../dataschema/IFloorPersistence'
import IFloorRepo, { BuildingFloorCount } from '../services/IRepos/IFloorRepo'
import { Floor } from '../domain/floor/floor'
import { FloorMap } from '../mappers/FloorMap'
import { FloorNumber } from '../domain/floor/floorNumber'
import Building from '../domain/building/building'
import { json } from 'body-parser'
import { IBuildingPersistence } from '../dataschema/IBuildingPersistence'
import { BuildingCode } from '../domain/building/buildingCode'

@Service()
export default class FloorRepo implements IFloorRepo {
    private models: any

    constructor(@Inject('floorSchema') private floorSchema: Model<IFloorPersistence & Document>,
                @Inject('buildingSchema') private buildingSchema: Model<IBuildingPersistence & Document>) { }

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(floor: Floor | string): Promise<boolean> {
        if (floor instanceof Floor) {
            const query = { buildingCode: floor.building.code.value, floorNumber: floor.floorNumber.value }
            const floorDocument = await this.floorSchema.findOne(query)
            return !!floorDocument === true
        } else {
            return false
        }
    }
    public async findByCodeNumber(buildingCode: BuildingCode, floorNumber: FloorNumber): Promise<Floor> {
        const query = { buildingCode: buildingCode.value, floorNumber: floorNumber.value }
        const floorDocument = await this.floorSchema.findOne(query)
        if (floorDocument != null) {
            return FloorMap.toDomain(floorDocument)
        } else return null

    }

    public async findByID(floorID: string): Promise<Floor> {
        const query = { domainID: floorID}
        const floorDocument = await this.floorSchema.findOne(query)
        if (floorDocument != null) {
            return FloorMap.toDomain(floorDocument)
        } else return null

    }

    public async findBuildingsByMinMaxFloors(min: number, max: number): Promise<BuildingFloorCount[]> {
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
          }
        ]

        return (await this.floorSchema.aggregate(aggregationPipeline as PipelineStage[]))
                .map(result => {
                    return {
                        buildingCode: BuildingCode.create(result._id).getValue(),
                        floorCount: result.floorCount
                    }
                })
    }

    public async save(floor: Floor): Promise<Floor> {
        const query = { buildingCode: floor.building.code.value, floorNumber: floor.floorNumber.value }

        const floorDocument = await this.floorSchema.findOne(query)
        try {

            // if (floorDocument === null) {
            const rawFloor: any = FloorMap.toPersistence(floor)
            const floorCreated = await this.floorSchema.create(rawFloor)
            return FloorMap.toDomain(floorCreated)
            // } else {
            //     floorDocument.floorNumber = floor.floorNumber.value
            //     floorDocument.description = floor.description.value
            //     floorDocument.buildingCode = floor.building.buildingCode.toString()

            //     await floorDocument.save()
            //     return floor
            // }
        } catch (err) {
            throw err
        }
    }

    async find(building: Building, floorNumber: FloorNumber): Promise<Floor> {
        const query = {
            buildingId: building.code.value,
            floorNumber: floorNumber.value,
        }

        const floor = await this.floorSchema.findOne(query)
        return FloorMap.toDomain(floor)
    }

    public async findAllInBuilding(building: Building): Promise<Floor[]> {
        const query = {
            buildingId: building.code.value,
        }

        const records = await this.floorSchema.find(query)

        if (records.length === 0) {
            return []
        }
        const passageList = await Promise.all(records.map(record => FloorMap.toDomain(record)))

        return passageList
    }
}