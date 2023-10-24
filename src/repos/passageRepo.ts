import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import { IPassagePersistence } from '../dataschema/IPassagePersistence'
import IPassageRepo from '../services/IRepos/IPassageRepo'
import { Passage } from '../domain/passage/passage'
import { PassageMap } from '../mappers/PassageMap'
import { Console, error } from 'console'
import Building from '../domain/building/building'
import { BuildingMap } from '../mappers/BuildingMap'
import { BuildingCode } from '../domain/building/buildingCode'
import { Floor } from '../domain/floor/floor'
import { IPassageDTO } from '../dto/IPassageDTO'

@Service()
export default class passageRepo implements IPassageRepo {
    private models: any

    constructor(@Inject('passageSchema') private passageSchema: Model<IPassagePersistence & Document>) {}

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(passage: Passage | string): Promise<boolean> {
        if (passage instanceof Passage) {
            const query = {
                floor1ID: passage.props.floor1.id.toString(),
                floor2ID: passage.props.floor2.id.toString(),
            }
            const passageDocument = await this.passageSchema.findOne(query)

            try {
                if (passageDocument === null) {
                    //ordem 1 nao existe
                    const query2 = {
                        floor1ID: passage.props.floor2.id.toString(),
                        floor2ID: passage.props.floor1.id.toString(),
                    }
                    const passageDocument2 = await this.passageSchema.findOne(query2)

                    if (passageDocument2 === null) {
                        //ordem 2 nao existe
                        return false //digo que existe
                    } else {
                        return true
                    }
                } else {
                    return true
                }
            } catch (error) {
                throw error
            }
        }
        return false
    }

    public async save(passage: Passage): Promise<Passage> {
        try {
            if (await this.exists(passage)) {
                return null

                // passageDocument.floor1ID = passage.props.floor1.id.toString()
                // passageDocument.floor2ID = passage.props.floor2.id.toString()

                // await passageDocument.save()
                // return passage
            }

            const rawPassage: any = PassageMap.toPersistence(passage)

            const passageCreated = await this.passageSchema.create(rawPassage)

            return PassageMap.toDomain(passageCreated)
        } catch (err) {
            throw err
        }
    }

    public async findAll(): Promise<Passage[]> {
        const records = await this.passageSchema.find()

        if (records.length === 0) {
            return [] // Return an empty array when there are no records
        }
        const passageList = await Promise.all(records.map(record => PassageMap.toDomain(record)))

        return passageList
    }
    public async passagesBetweenBuildings(
        dto: IPassageDTO[],
        codex: BuildingCode,
        codey: BuildingCode,
    ): Promise<Passage[]> {
        try {
            const buildingCode1 = codex.value
            const buildingCode2 = codey.value

            const passages = dto.find(
                (info => info.floor1.buildingCode === buildingCode1) &&
                    (info => info.floor2.buildingCode === buildingCode2),
            )

            const doc = await this.passageSchema.find(passages)

            const passageList = await Promise.all(doc.map(record => PassageMap.toDomain(record)))

            return passageList
        } catch (error) {
            throw error
        }
    }
}
