import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import { IFloorPersistence } from '../dataschema/IFloorPersistence'
import IFloorRepo from '../services/IRepos/IFloorRepo'
import { Floor } from '../domain/floor/floor'
import { FloorId } from '../domain/floor/floorId'
import { FloorMap } from '../mappers/FloorMap'
import { FloorNumber } from '../domain/floor/floorNumber'
import { json } from 'body-parser'

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
            console.log("Searching Number",floor.floorNumber.value)
            console.log("Searching Building",floor.building.code.value)
            const floorDocument = await this.floorSchema.findOne(query)
            if(!!floorDocument===true){//encontrou
                console.log("Has id")
                console.log("Floor document",JSON.stringify(floorDocument))
                console.log("ID",floorDocument.domainId.valueOf())
            }
            return !!floorDocument === true
        } else {
            return false
        }
    }

    public async save(floor: Floor): Promise<Floor> {
        const query = { buildingCode: floor.building.code.value, floorNumber: floor.floorNumber.value }

        const floorDocument = await this.floorSchema.findOne(query)
        // if(!this.exists(floor)){
        //     console.log("Floor to create",floor)
        //     const rawFloor: any = FloorMap.toPersistence(floor)
        //     const floorCreated = await this.floorSchema.create(rawFloor)
        //     return FloorMap.toDomain(floorCreated)

        // }else{
        //     console.log("Floor already exists")
        //     return null;
        // }

        try {

            console.log(JSON.stringify(floor))
            // if (floorDocument === null) {
                console.log("Going to save")
                const rawFloor: any = FloorMap.toPersistence(floor)
                const floorCreated = await this.floorSchema.create(rawFloor)
                return FloorMap.toDomain(floorCreated)
            // } else {
            //     floorDocument.floorNumber = floor.floorNumber.value
            //     floorDocument.description = floor.description.value
            //     floorDocument.buildingCode = floor.building.buildingCode.toString()

            //     console.log("Floor Before", floor.id)
            //     await floorDocument.save()
            //     console.log("Floor After", floor.id)
            //     return floor
            // }
        } catch (err) {
            throw err
        }
    }
}
