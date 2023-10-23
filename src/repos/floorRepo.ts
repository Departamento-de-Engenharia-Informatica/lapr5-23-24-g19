import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import { IFloorPersistence } from '../dataschema/IFloorPersistence'
import IFloorRepo from '../services/IRepos/IFloorRepo'
import { Floor } from '../domain/floor/floor'
import { FloorId } from '../domain/floor/floorId'
import { FloorMap } from '../mappers/FloorMap'
import { FloorNumber } from '../domain/floor/floorNumber'
import { json } from 'body-parser'
import Building from '../domain/building/building'

@Service()
export default class FloorRepo implements IFloorRepo {
    private models: any

    constructor(@Inject('floorSchema') private floorSchema: Model<IFloorPersistence & Document>) { }

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

}
