import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import { IBuildingPersistence } from '../../dataschema/mongo/IBuildingPersistence'
import IBuildingRepo from '../../services/IRepos/IBuildingRepo'
import Building from '../../domain/building/building'
import { BuildingId } from '../../domain/building/buildingId'
import { BuildingMap } from '../../mappers/BuildingMap'
import { BuildingCode } from '../../domain/building/code'

@Service()
export default class BuildingRepo implements IBuildingRepo {
    private models: any

    constructor(
        @Inject('buildingSchema')
        private buildingSchema: Model<IBuildingPersistence & Document>,
    ) {}

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(building: Building): Promise<boolean> {
        const idX = building instanceof BuildingId ? building.id.toValue() : building

        const query = { domainId: idX }
        const buildingDocument = await this.buildingSchema.findOne(query)

        return !!buildingDocument === true
    }

    public async save(building: Building): Promise<Building> {
        const query = { code: building.code.value }

        const buildingDocument = await this.buildingSchema.findOne(query)

        try {
            if (buildingDocument === null) {
                const rawBuilding: any = BuildingMap.toPersistence(building)
                const buildingCreated = await this.buildingSchema.create(rawBuilding)

                return BuildingMap.toDomain(buildingCreated)
            } else {
                buildingDocument.code = building.code.value
                buildingDocument.name = building.name?.value
                buildingDocument.description = building.description?.value

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

    async findByCode(code: BuildingCode): Promise<Building> {
        const record = await this.buildingSchema.findOne({ code: code.value })

        if (record != null) {
            return BuildingMap.toDomain(record)
        } else {
            return null
        }
    }

    async findAll(): Promise<Building[]> {
        const doc = await this.buildingSchema.find()

        if (doc.length === 0) {
            return []
        }

        return doc.map((b) => BuildingMap.toDomain(b))
    }
}
