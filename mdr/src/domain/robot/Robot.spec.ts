import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { beforeEach, describe, it } from 'mocha'
import Robot from './Robot'
import { RobotCode } from './code'
import { RobotNickname } from './nickname'
import { RobotSerialNumber } from './serialNumber'
import RobotType from '../robotType/robotType'
import { RobotTypeCode } from '../robotType/robotTypeCode'
import { RobotTypeBrand } from '../robotType/robotTypeBrand'
import { RobotTypeModel } from '../robotType/robotTypeModel'
import { TaskType } from '../robotType/taskType'
import { Result } from '../../core/logic/Result'
import { Description } from '../description'
import { RobotState } from './state'

describe('Robot', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    function stubToType<K>(klass: K) {
        sinon.stub(klass, 'toType' as keyof K).returns(Result.ok<K>({} as K))
    }

    let code: RobotCode, nick: RobotNickname, serial: RobotSerialNumber, type: RobotType

    beforeEach(() => {
        stubCreate(RobotCode)
        stubCreate(RobotNickname)
        stubCreate(RobotSerialNumber)
        stubCreate(RobotType)

        code = RobotCode.create('RobotCode').getValue()
        nick = RobotNickname.create('MarcoBot').getValue()
        serial = RobotSerialNumber.create('ABC').getValue()

        stubCreate(RobotTypeCode)
        stubCreate(RobotTypeBrand)
        stubCreate(RobotTypeModel)
        stubToType(TaskType)

        type = RobotType.create({
            code: RobotTypeCode.create('RobotTypeCode').getValue(),
            brand: RobotTypeBrand.create('BMW').getValue(),
            model: RobotTypeModel.create('MegaRobot').getValue(),
            taskType: ['a' as TaskType, 'b' as TaskType],
        }).getValue()
    })

    afterEach(sinon.restore)

    it('allows an optional description', () => {
        const description = Description.create('RandomRobot').getValue()

        let result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,

            description,
        })

        assert.isOk(result.isSuccess)

        result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,

            description: undefined,
        })

        assert.isOk(result.isSuccess)
    })

    it('cannot create a robot without a code', () => {
        let result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        result = Robot.create({
            code: undefined,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isNotOk(result.isSuccess)
    })

    it('cannot create a robot without a nickname', () => {
        let result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        result = Robot.create({
            code,
            nickname: undefined,
            serialNumber: serial,
            type,
        })

        assert.isNotOk(result.isSuccess)
    })

    it('cannot create a robot without a serial number', () => {
        let result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        result = Robot.create({
            code,
            nickname: nick,
            serialNumber: undefined,
            type,
        })

        assert.isNotOk(result.isSuccess)
    })

    it('cannot create a robot without a type', () => {
        let result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type: undefined,
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should be able to inhibit robot', () => {
        const result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        let value = result.getValue().state
        assert.equal(value, RobotState.ENABLED)

        result.getValue().inhibit()

        value = result.getValue().state
        assert.equal(value, RobotState.DISABLED)
    })

    it('should be able to get code value', () => {
        const result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        const value = result.getValue().code

        assert.equal(value, result.getValue().code)
    })

    it('should be able to get nickname value', () => {
        const result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        const value = result.getValue().nickname

        assert.equal(value, result.getValue().nickname)
    })

    it('should be able to get serial number value', () => {
        const result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        const value = result.getValue().serialNumber

        assert.equal(value, result.getValue().serialNumber)
    })

    it('should be able to get type value', () => {
        const result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        const value = result.getValue().type

        assert.equal(value, result.getValue().type)
    })

    it('should be able to get description value', () => {
        const result = Robot.create({
            code,
            nickname: nick,
            serialNumber: serial,
            type,
        })

        assert.isOk(result.isSuccess)

        const value = result.getValue().description

        assert.equal(value, result.getValue().description)
    })
})
