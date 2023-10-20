import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import { IBuildingPersistence } from '../dataschema/IBuildingPersistence'
import IBuildingRepo from '../services/IRepos/IBuildingRepo'
import { Building } from '../domain/building/building'
import { BuildingId } from '../domain/building/buildingId'
import { BuildingMap } from '../mappers/BuildingMap'

@Service()
export default class BuildingRepo implements IBuildingRepo {
    private models: any

    constructor(@Inject('buildingSchema') private buildingSchema: Model<IBuildingPersistence & Document>) {}

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(buildingId: BuildingId | string): Promise<boolean> {
        const idX = buildingId instanceof BuildingId ? buildingId.id.toValue() : buildingId

        const query = { domainId: idX }
        const buildingDocument = await this.buildingSchema.findOne(query)

        return !!buildingDocument === true
    }

    public async save(building: Building): Promise<Building> {
        const query = { domainId: building.id.toString() }

        const buildingDocument = await this.buildingSchema.findOne(query)

        try {
            if (buildingDocument === null) {
                const rawBuilding: any = BuildingMap.toPersistence(building)

                const buildingCreated = await this.buildingSchema.create(rawBuilding)

                return BuildingMap.toDomain(buildingCreated)
            } else {
                buildingDocument.code = building.code
                buildingDocument.name = building.name
                buildingDocument.description = building.description

                const { length, width } = building.maxFloorDimensions
                buildingDocument.maxFloorLength = length
                buildingDocument.maxFloorWidth = width

                await buildingDocument.save()

                return building
            }
        } catch (err) {
            throw err
        }
    }

    public async findById(buildingId: BuildingId | string): Promise<Building> {
        const idX = buildingId instanceof BuildingId ? buildingId.id.toValue() : buildingId

        const query = { domainId: idX }
        const buildingRecord = await this.buildingSchema.findOne(query)

        if (buildingRecord != null) {
            return BuildingMap.toDomain(buildingRecord)
        } else return null
    }
}
