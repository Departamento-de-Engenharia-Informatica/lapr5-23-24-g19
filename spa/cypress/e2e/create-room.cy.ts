function loginViaAuth0Ui(username: string, password: string) {
    cy.origin(
        Cypress.env('auth_domain'),
        { args: { username, password } },
        ({ username, password }) => {
            cy.get('input#1-email').type(username)
            cy.get('input#1-password').type(password, { log: false })
            cy.get('button[type="submit"]')
                .should('be.visible')
                .should('contain.text', 'Log In')
                .should('not.be.disabled')
                .should('not.be.hidden')
                .click()
        },
    )
}

describe('Room Form e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/buildings', {
            body: [
                {
                    code: 'P',
                    description: 'Civil2',
                },
                {
                    code: 'O',
                    description: 'Informatic',
                },
            ],
        }).as('getBuildings')
        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('/campus/rooms/create')

        const log = Cypress.log({
            displayName: 'AUTH0 LOGIN',
            message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
            // @ts-ignore
            autoEnd: false,
        })
        log.snapshot('before')

        loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'))

        log.snapshot('after')
        log.end()
    })

    it('has the correct title', () => {
        cy.title().should('equal', 'Create Room')
    })

    it('should have empty initial values', () => {
        cy.get('#selectedBuilding').should('have.value', null)
        cy.get('#selectedFloor').should('have.value', null)
        cy.get('#name').should('have.value', '')
        cy.get('#description').should('have.value', '')
        cy.get('#category').should('have.value', null)
        cy.get('#length').should('have.value', '')
        cy.get('#width').should('have.value', '')
        cy.get('#x').should('have.value', '')
        cy.get('#y').should('have.value', '')
    })

    it('should submit the form successfully and display created room', () => {
        cy.wait('@getBuildings')

        cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
            body: [
                {
                    buildingCode: 'P',
                    floorNumber: 1,
                    description: 'Physics Labs',
                },
                {
                    buildingCode: 'P',
                    floorNumber: 2,
                    description: 'Physics Labs',
                },
                {
                    buildingCode: 'P',
                    floorNumber: 3,
                    description: 'Physics Labs',
                },
            ],
        }).as('getFloors')

        cy.get('#selectedBuilding').select('P')
        cy.wait('@getFloors')

        const buildingCode = 'P'
        const floorNumber = '1'
        const name = 'RoomName'
        const description = 'This is a test room.'
        const category = 'GABINETE'
        const length = 10
        const width = 15
        const xPosition = 5
        const yPosition = 8

        cy.get('#selectedBuilding').select(buildingCode)
        cy.get('#selectedFloor').select(floorNumber)
        cy.get('#name').type(name)
        cy.get('#description').type(description)
        cy.get('#category').select(category)
        cy.get('#length').type(length.toString())
        cy.get('#width').type(width.toString())
        cy.get('#x').type(xPosition.toString())
        cy.get('#y').type(yPosition.toString())

        cy.intercept('POST', 'http://localhost:4000/api/buildings/P/floors/1/rooms', {
            statusCode: 200,
            body: {
                name: name,
                buildingCode: buildingCode,
                floorNumber: floorNumber,
                description: description,
                category: category,
                dimensions: {
                    length,
                    width,
                },
                positions: {
                    x: xPosition,
                    y: yPosition,
                },
            },
        }).as('createRoom')

        cy.get('button[type="submit"]').click()
        cy.wait('@createRoom')

        cy.get('.room-card h2').should('contain', 'Created Room')
        cy.get('.room-card p').should('have.length', 7)

        cy.get('.room-card p').contains(`Name: ${name}`)
        cy.get('.room-card p').contains(`Building Code: ${buildingCode}`)
        cy.get('.room-card p').contains(`Floor: ${floorNumber}`)
        cy.get('.room-card p').contains(`Description: ${description}`)
        cy.get('.room-card p').contains(`Category: ${category}`)
        cy.get('.room-card p').contains(
            `Dimensions: Length - ${length}, Width - ${width}`,
        )
        cy.get('.room-card p').contains(`Positions: X - ${xPosition}, Y - ${yPosition}`)
    })
})
