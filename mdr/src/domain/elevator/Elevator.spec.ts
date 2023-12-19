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
import { FloorNumber } from '../floor/floorNumber'

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

    it('should be able to get building', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
        })

        assert.equal(building, result.getValue().building)
    })

    it('should be able to get identifier', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
        })

        assert.equal(identifier, result.getValue().identifier)
    })

    it('should be able to get floors', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
        })

        assert.equal(floor, result.getValue().floors[0])
    })

    it('should be able to get brand', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const brand = Brand.create('Android').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: brand,
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
        })

        assert.equal(brand, result.getValue().brand)
    })

    it('should be able to get model', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const brand = Brand.create('Android').getValue()
        const model = Model.create('Version2').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: brand,
            model: model,
            serialNumber: SerialNumber.create('').getValue(),
        })

        assert.equal(model, result.getValue().model)
    })

    it('should be able to get serial number', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const serialNumber = SerialNumber.create('SERIAL123').getValue()
        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: serialNumber,
        })

        assert.equal(serialNumber, result.getValue().serialNumber)
    })

    it('should be able to get description', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const description = Description.create('TestDescription').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),

            description,
        })

        assert.equal(description, result.getValue().description)
    })

    it('should be able to set brand', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const brand = Brand.create('Android').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
        })

        result.getValue().brand = brand

        assert.equal(brand, result.getValue().brand)
    })

    it('should be able to set model', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const model = Model.create('iOS').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
        })

        result.getValue().model = model

        assert.equal(model, result.getValue().model)
    })

    it('should be able to set serial number', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const serialNumber = SerialNumber.create('SERIAL123').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
        })

        result.getValue().serialNumber = serialNumber

        assert.equal(serialNumber, result.getValue().serialNumber)
    })

    it('should be able to set description', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const description = Description.create('description').getValue()
        const newDescription = Description.create('newDescription').getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),

            description,
        })

        result.getValue().description = newDescription

        assert.equal(newDescription, result.getValue().description)
    })

    it('should be able to set floors', () => {
        stubCreate(Brand)
        stubCreate(Model)
        stubCreate(SerialNumber)

        const newFloor = Floor.create({
            building: building,
            floorNumber: FloorNumber.create(4).getValue(),
        }).getValue()

        let result = Elevator.create({
            building,
            identifier,
            floors: [floor],
            brand: Brand.create('').getValue(),
            model: Model.create('').getValue(),
            serialNumber: SerialNumber.create('').getValue(),
        })

        result.getValue().floors = [newFloor]

        assert.equal(newFloor, result.getValue().floors[0])
    })
})
