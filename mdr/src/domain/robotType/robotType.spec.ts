import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'
import RobotType from './robotType'
import { RobotTypeCode } from './robotTypeCode'
import { RobotTypeBrand } from './robotTypeBrand'
import { RobotTypeModel } from './robotTypeModel'
import { TaskType } from './taskType'
import { Result } from '../../core/logic/Result'

describe('RobotType', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    beforeEach(() => {})

    afterEach(sinon.restore)

    it('should create a valid robot type', () => {
        const code = RobotTypeCode.create('R001').getValue()
        const brand = RobotTypeBrand.create('RoboTech').getValue()
        const model = RobotTypeModel.create('ModelX').getValue()
        const taskType = [TaskType.toType('delivery'), TaskType.toType('Surveillance')]

        const result = RobotType.create({
            code,
            brand,
            model,
            taskType,
        })

        assert.isOk(result.isSuccess)
    })

    it('should fail when creating a robot type with null or undefined code', () => {
        stubCreate(RobotTypeBrand)
        stubCreate(RobotTypeModel)
        sinon.stub(TaskType, 'toType').returns('asdasd' as TaskType)

        const result = RobotType.create({
            code: undefined,
            brand: RobotTypeBrand.create('').getValue(),
            model: RobotTypeModel.create('').getValue(),
            taskType: [TaskType.toType('delivery'), TaskType.toType('Surveillance')],
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a robot type with null or undefined brand', () => {
        stubCreate(RobotTypeCode)
        stubCreate(RobotTypeModel)
        sinon.stub(TaskType, 'toType').returns('asdasd' as TaskType)

        const result = RobotType.create({
            code: RobotTypeCode.create('').getValue(),
            brand: undefined,
            model: RobotTypeModel.create('').getValue(),
            taskType: [TaskType.toType('delivery'), TaskType.toType('Surveillance')],
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a robot type with null or undefined model', () => {
        stubCreate(RobotTypeCode)
        stubCreate(RobotTypeBrand)
        sinon.stub(TaskType, 'toType').returns('asdasd' as TaskType)

        const result = RobotType.create({
            code: RobotTypeCode.create('').getValue(),
            brand: RobotTypeBrand.create('').getValue(),
            model: undefined,
            taskType: [TaskType.toType('delivery'), TaskType.toType('Surveillance')],
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a robot type with null or undefined task type', () => {
        stubCreate(RobotTypeCode)
        stubCreate(RobotTypeBrand)
        stubCreate(RobotTypeModel)
        sinon.stub(TaskType, 'toType').returns('asdasd' as TaskType)

        const result = RobotType.create({
            code: RobotTypeCode.create('').getValue(),
            brand: RobotTypeBrand.create('').getValue(),
            model: RobotTypeModel.create('').getValue(),
            taskType: undefined,
        })

        assert.isNotOk(result.isSuccess)
    })
})
