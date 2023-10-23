import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'

import { Result } from '../../core/logic/Result'

import Building, { BuildingProps } from '../building/building'
import { Floor, FloorProps } from '../floor/floor'

import Elevator from './Elevator'
import { ElevatorIdentifier as Identifier } from './identifier'
import { ElevatorBrand as Brand } from './brand'
import { ElevatorModel as Model } from './model'
import { ElevatorDescription as Description } from './description'
import { ElevatorSerialNumber as SerialNumber } from './serialNumber'

describe('Elevator', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    let building: Building, floor: Floor, identifier: Identifier

    beforeEach(() => {
        stubCreate(Building)
        stubCreate(Floor)
        stubCreate(Identifier)

        building = Building.create({} as BuildingProps).getValue()
        floor = Floor.create({} as FloorProps).getValue()
        identifier = Identifier.create(1).getValue()
    })

    afterEach(sinon.restore)

    it('must have a model, if brand is specified', () => {
        const brand = Brand.create('Schindler').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],

            brand,
            model: undefined,
        })

        assert.isNotOk(result.isSuccess)

        const model = Model.create('3300').getValue()

        result = Elevator.create({
            building,
            identifier,
            floors: [floor],

            brand,
            model,
        })

        assert.isOk(result.isSuccess)

        result = Elevator.create({
            building,
            identifier,
            floors: [floor],

            brand: undefined,
            model,
        })

        assert.isOk(result.isSuccess)
    })

    it('allows an optional brand', () => {
        stubCreate(Model)
        stubCreate(SerialNumber)
        stubCreate(Description)

        const brand = Brand.create('Schindler').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
            description: Description.create('').getValue(),

            // prop in testing
            brand,
        })

        assert.isOk(result.isSuccess)

        result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
            description: Description.create('').getValue(),

            // prop in testing
            brand: undefined,
        })

        assert.isOk(result.isSuccess)
    })

    it('allows an optional serial number', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(Description)

        const serialNumber = SerialNumber.create('X123WQL').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            description: Description.create('').getValue(),

            // prop in testing
            serialNumber,
        })

        assert.isOk(result.isSuccess)

        result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            description: Description.create('').getValue(),

            // prop in testing
            serialNumber: undefined,
        })

        assert.isOk(result.isSuccess)
    })

    it('allows an optional description', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const description = Description.create('SomeElevator').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),

            // prop in testing
            description,
        })

        assert.isOk(result.isSuccess)

        result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),

            // prop in testing
            description: undefined,
        })

        assert.isOk(result.isSuccess)
    })
})
