import { Inject, Service } from "typedi";
import { Model as MongoModel, Document } from 'mongoose'
import config from "../../../config";
import IElevatorPersistence from "../../dataschema/mongo/IElevatorPersistence";
import Building from "../../domain/building/building";
import Elevator from "../../domain/elevator/Elevator";
import IElevatorRepo, { ElevatorDataMap } from "../../services/IRepos/IElevatorRepo";
import MongoElevatorDataMap from "./dataMapper/ElevatorDataMap";
import { ElevatorIdentifier } from "../../domain/elevator/identifier";

import IBuildingRepo from "../../services/IRepos/IBuildingRepo";
import IFloorRepo from "../../services/IRepos/IFloorRepo";

@Service()
export default class ElevatorRepo implements IElevatorRepo {
    private readonly mapper: ElevatorDataMap<IElevatorPersistence>

    constructor(
        @Inject(config.schemas.elevator.name) private schema: MongoModel<IElevatorPersistence & Document>,

        // scuffed; this is not supposed to be needed
        @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
        @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    ) {
        // this shouldn't be needed; check class impl for more info
        this.mapper = new MongoElevatorDataMap(this.buildingRepo, this.floorRepo)
    }

    async save(t: Elevator): Promise<Elevator> {
        const query: Partial<IElevatorPersistence> = {
            building: t.building.code.value,
            identifier: t.identifier.value,
        }

        const doc = await this.schema.findOne(query)
        try {
            if (!doc) {
                const raw = await this.mapper.toPersistence(t)
                const elevator = await this.schema.create(raw)

                return this.mapper.toDomain(elevator)
            } else {
                doc.brand = t.brand?.value
                doc.model = t.model?.value
                doc.serialNumber = t.serialNumber?.value
                doc.description = t.description?.value

                await doc.save()
                return t
            }
        } catch (err) {
            throw err
        }

    }
    async exists(t: Elevator): Promise<boolean> {
        return this.existsInBuilding(t.building, t.identifier)
    }

    async inBuilding(building: Building): Promise<Elevator[]> {
        const result = await this.schema.find({
            building: building.code.value,
        })
        return Promise.all(result.map(this.mapper.toDomain)) ?? []
    }

    async existsInBuilding(building: Building, identifier: ElevatorIdentifier): Promise<boolean> {

        const query: Partial<IElevatorPersistence> = {
            building: building.code.value,
            identifier: identifier.value,
        }

        const doc = await this.schema.findOne(query)

        return !!doc === true
    }
}
