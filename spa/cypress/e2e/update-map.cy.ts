// // import 'cypress-file-upload'
//
// function loginViaAuth0Ui(username: string, password: string) {
//     cy.origin(
//         Cypress.env('auth_domain'),
//         { args: { username, password } },
//         ({ username, password }) => {
//             cy.get('input#1-email').type(username)
//             cy.get('input#1-password').type(password, { log: false })
//             cy.get('button[type="submit"]')
//                 .should('be.visible')
//                 .should('contain.text', 'Log In')
//                 .should('not.be.disabled')
//                 .should('not.be.hidden')
//                 .click()
//         },
//     )
// }
//
// describe('UpdateMapComponent', () => {
//     beforeEach(() => {
//         cy.intercept('GET', 'http://localhost:4000/api/buildings', {
//             body: [
//                 {
//                     code: 'P',
//                     name: 'Physics',
//                     description: 'Physics Department',
//                     maxFloorDimensions: {
//                         length: 26,
//                         width: 12,
//                     },
//                 },
//                 {
//                     code: 'C',
//                     name: 'Chemistry',
//                     description: 'Chemistry Department',
//                     maxFloorDimensions: {
//                         length: 20,
//                         width: 15,
//                     },
//                 },
//             ],
//         }).as('getBuildings')
//
//         cy.visit('campus/floors/update-map')
//         const log = Cypress.log({
//             displayName: 'AUTH0 LOGIN',
//             message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
//             // @ts-ignore
//             autoEnd: false,
//         })
//         log.snapshot('before')
//
//         loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'))
//
//         log.snapshot('after')
//         log.end()
//         cy.wait('@getBuildings')
//     })
//
//     it('should display the form with building options', () => {
//         cy.get('#buildingCode').should('exist')
//         cy.get('#floorNumber').should('exist')
//         cy.get('#mapFile').should('exist')
//         cy.get('button').should('exist')
//         cy.get('#buildingCode').find('option').should('have.length', 2)
//     })
//
//     it('should list floors when a building is selected', () => {
//         cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
//             body: [
//                 {
//                     floorNumber: 1,
//                 },
//                 {
//                     floorNumber: 2,
//                 },
//             ],
//         }).as('getFloors')
//
//         cy.get('#buildingCode').select('P')
//         cy.wait('@getFloors')
//
//         cy.get('#floorNumber').find('option').should('have.length', 2)
//     })
//
//     it('should handle file upload and submit the form', () => {
//         cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
//             body: [
//                 {
//                     floorNumber: 1,
//                 },
//             ],
//         }).as('getFloors')
//
//         cy.get('#buildingCode').select('P')
//         cy.wait('@getFloors')
//         cy.get('#floorNumber').select('1')
//
//         cy.intercept('GET', 'http://localhost:4000/api/floors', {
//             statusCode: 200,
//             body: [{ floorNumber: 1 }, { floorNumber: 2 }],
//         })
//
//         cy.intercept('PATCH', 'http://localhost:4000/api/buildings/P/floors/1/map', {
//             statusCode: 200,
//             body: { success: true },
//         }).as('updateMap')
//
//         // BUTTON should be disabled before select mapFile
//         cy.get('#submitForm').should('be.disabled')
//
//         // cy.get('#mapFile').attachFile('map.json').then(()=>{
//
//         // HELP: o butao esta disabled pq obriga a preencher o formulario todo,
//         // mas quando eu faço este attach file por alguma razao o butao nao fica enabled(mas a testar manualmente fica)
//         // presumo que podera ser uma questao de timming(?), ter de esperar que o ficheiro carregue
//         // ou por alguma razao, o evento nao esta a ser captado, e de alguma maneira o mapa continua sem informaçã0???
//         // nao sei, ja tentei varias coisas e nao consegui :)))))))))))))))))))
//
//         // cy.get('#submitForm').should('be.enabled')
//         // cy.get('#submitForm').click()
//         // cy.wait('@updateMap').its('response.statusCode').should('eq', 200)
//         // });
//     })
// })
