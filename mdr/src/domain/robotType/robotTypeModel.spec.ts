import { assert } from 'chai'
import { describe, it } from 'mocha'
import { RobotTypeModel } from './robotTypeModel'

describe('RobotTypeModel', () => {
    it('should create a valid robot type model', () => {
        const validModelValue = 'RoboX-2000'
        const robotTypeModel = RobotTypeModel.create(validModelValue)

        assert.isOk(robotTypeModel.isSuccess)
    })

    it('should fail when creating a robot type model with null or undefined value', () => {
        const robotTypeModel1 = RobotTypeModel.create(null)
        assert.isNotOk(robotTypeModel1.isSuccess)

        const robotTypeModel2 = RobotTypeModel.create(undefined)
        assert.isNotOk(robotTypeModel2.isSuccess)
    })

    it('should fail when the model exceeds the maximum length', () => {
        const longModel = 'A'.repeat(101) // Exceeds the maximum length of 100 characters
        const robotTypeModel = RobotTypeModel.create(longModel)

        assert.isNotOk(robotTypeModel.isSuccess)
    })

    it('should trim leading and trailing whitespace from the value', () => {
        const modelValue = '   RoboX-2000   '
        const robotTypeModel = RobotTypeModel.create(modelValue)

        assert.isOk(robotTypeModel.isSuccess)
        assert.equal(robotTypeModel.getValue().value, 'RoboX-2000')
    })
})
