// for args validation
import { isInteger, isNil } from 'lodash';
// for database connection
import * as mysql from 'mysql';
// for args parse
import program from 'commander';

async function main() {
    try {
        program
            .version('0.1.0')
            .option('-q,--question <question_id>', 'id of the question to choose', parseInt)
            .option('-p,--params <params_json>', 'json string format params to pass', JSON.parse)
            .parse(process.argv);
    } catch (e) {
        console.log(e);
        console.error('invalid args.');
        process.exit(-1);
    }

    const connection = mysql.createConnection({
        host: '192.168.6.52',
        user: 'root',
        database: 'company',
        password: 'test'
    });

    const exit = async () => {
        await connection.end();
        process.exit(0);
    };

    try {
        await connection.connect();
        console.log(`connected.`);
    } catch (err) {
        console.error(`failed to connect: ${err}`);
        await exit();
    }

    const wrappedQuery = async function (sql: string, args: any[]) {
        return new Promise((resolve, reject) => {
            connection.query(sql, args, (err, results) => {
                if (err) {
                    return reject(err);
                } else {
                    return resolve(results);
                }
            });
        });
    } as (sql: string, args: any[]) => Promise<any[]>;

    const actions = [
        // no 1
        async (queries: { pno: string }) => {
            if ((typeof queries.pno) != 'string') {
                console.error(`please input correct string as pno.`);
                await exit();
            }
            try {
                const results: { essn: string }[] = await wrappedQuery(
                    'SELECT essn FROM works_on WHERE pno = ?',
                    [queries.pno],
                );
                console.log('results are：');
                for (const res of results) {
                    console.log(`employee ${res.essn} works on project ${queries.pno}.`);
                }
            } catch (e) {
                console.error(`query failed: ${e}`);
            } finally {
                await exit();
            }
        },
        // no 2
        async (queries: { pname: string }) => {
            if ((typeof queries.pname) != 'string') {
                console.error(`please input correct string as pname.`);
                await exit();
            }
            try {
                const results: { ename: string }[] = await wrappedQuery(
                    `SELECT DISTINCT ename FROM employee WHERE essn IN (SELECT DISTINCT essn FROM works_on NATURAL JOIN project WHERE pname = ?)`,
                    [queries.pname],
                );
                console.log('results are：');
                for (const res of results) {
                    console.log(`employee ${res.ename} works on project ${queries.pname}.`);
                }
            } catch (e) {
                console.error(`query failed: ${e}`);
            } finally {
                await exit();
            }
        },
        // no 3
        async (queries: { dname: string }) => {
            if ((typeof queries.dname) != 'string') {
                console.error(`please input correct string as dname`);
                await exit();
            }
            try {
                const results: { ename: string, address: string }[] = await wrappedQuery(
                    'SELECT DISTINCT ename, address FROM employee NATURAL JOIN department WHERE dname = ?',
                    [queries.dname],
                );
                console.log('results are：');
                for (const res of results) {
                    console.log(`employee ${res.ename} works on project ${queries.dname}, and his/her address is ${res.address}.`);
                }
            } catch (e) {
                console.error(`query failed: ${e}`);
            } finally {
                await exit();
            }
        },
        // no 4
        async (queries: { dname: string, salary: number }) => {
            if ((typeof queries.dname) != 'string') {
                console.error(`please input correct string as dname`);
                await exit();
            }
            if (!isInteger(queries.salary)) {
                console.log(`please input correct integer as salary`);
                await exit();
            }
            try {
                const results: { ename: string, address: string }[] = await wrappedQuery(
                    'SELECT DISTINCT ename, address FROM employee NATURAL JOIN department WHERE dname = ? AND salary < ?',
                    [queries.dname, queries.salary],
                );
                console.log('results are：');
                for (const res of results) {
                    console.log(`employee ${res.ename} works on project ${queries.dname}.\n` +
                        `and his/her address is ${res.address}, and his/her salary is lower than ${queries.salary}.`);
                }
            } catch (e) {
                console.error(`query failed: ${e}`);
            } finally {
                await exit();
            }
        },
        // no 5
        async (queries: { pno }) => {
            if ((typeof queries.pno) != 'string') {
                console.error(`please input correct string as pno`);
                await exit();
            }
            try {
                const results: { ename: string }[] = await wrappedQuery(
                    `SELECT employee.ename
                        FROM employee
                        WHERE employee.essn
                                NOT IN (
                                SELECT essn as essn
                                FROM works_on
                                WHERE pno = ?
                              );`,
                    [queries.pno],
                );
                console.log('results are：');
                for (const res of results) {
                    console.log(`employee ${res.ename} doesn't work on project ${queries.pno}.`);
                }
            } catch (e) {
                console.error(`query failed: ${e}`);
            } finally {
                await exit();
            }
        },
        // no 6
        async (queries: { ename }) => {
            if ((typeof queries.ename) != 'string') {
                console.error(`please input correct string as ename`);
                await exit();
            }
            try {
                const results: { ename: string, dname: string }[] = await wrappedQuery(
                    `SELECT DISTINCT ename, dname
                        FROM employee
                               NATURAL JOIN department
                        WHERE superssn = (
                          SELECT essn
                          FROM employee
                          WHERE ename = ?
                          );`,
                    [queries.ename],
                );
                console.log('results are：');
                for (const res of results) {
                    console.log(`employee ${res.ename} work on ${res.dname} and his/hers boss is ${queries.ename}.`);
                }
            } catch (e) {
                console.error(`query failed: ${e}`);
            } finally {
                await exit();
            }
        },
        // no 7
        async (queries: { pno1, pno2 }) => {
            if ((typeof queries.pno1) != 'string') {
                console.error(`please input correct string as pno1`);
                await exit();
            }
            if ((typeof queries.pno2) != 'string') {
                console.error(`please input correct string as pno2`);
                await exit();
            }
            if (queries.pno1 == queries.pno2) {
                console.error(`please input two different project`);
            }
            try {
                const results: { essn: string }[] = await wrappedQuery(
                    `SELECT essn
                        FROM (
                               (SELECT essn
                                FROM works_on
                                WHERE pno = ? )
                               UNION ALL
                               (
                                 SELECT essn
                                 FROM works_on
                                 WHERE pno = ?
                               )
                             )
                               AS union_essn
                        GROUP BY (essn)
                        HAVING COUNT(essn) >= 2;`,
                    [queries.pno1, queries.pno2],
                );
                console.log('results are：');
                for (const res of results) {
                    console.log(`employee ${res.essn} at least work on project ${queries.pno1} and ${queries.pno2}.`);
                }
            } catch (e) {
                console.error(`query failed: ${e}`);
            } finally {
                await exit();
            }
        },
        // no 8
        async (queries: { salary }) => {
            if (!isInteger(queries.salary)) {
                console.error(`please input correct integer as salary`);
                await exit();
            }
            try {
                const results: { dname: string }[] = await wrappedQuery(
                    `SELECT DISTINCT department.dname
                        FROM department,
                             (
                               SELECT employee.dno
                               FROM employee
                               GROUP BY (employee.dno)
                               HAVING AVG(employee.salary) < ?
                             ) AS dep_avg_salary
                        WHERE department.dno = dep_avg_salary.dno;`,
                    [queries.salary],
                );
                console.log('results are：');
                for (const res of results) {
                    console.log(`the average salary of department ${res.dname} is lower than ${queries.salary}.`);
                }
            } catch (e) {
                console.error(`query failed: ${e}`);
            } finally {
                await exit();
            }
        },
        // no 9
        async (queries: { n, hours }) => {
            if (!isInteger(queries.n)) {
                console.error(`please input correct integer as n`);
                await exit();
            }
            if (!isInteger(queries.hours)) {
                console.error(`please input correct integer as hours`);
                await exit();
            }
            try {
                const results: { ename: string }[] = await wrappedQuery(
                    `SELECT DISTINCT employee.ename
                            FROM employee,
                                 (
                                   SELECT works_on.essn
                                   FROM works_on
                                   GROUP BY works_on.essn
                                   HAVING COUNT(works_on.pno) >= ?
                                      AND SUM(works_on.hours) <= ?
                                 ) AS selected_employee
                            WHERE employee.essn = selected_employee.essn;`,
                    [queries.n, queries.hours],
                );
                console.log('results are：');
                for (const res of results) {
                    console.log(`employee ${res.ename} work project is greater than or equal to ${queries.hours},\n` +
                        `and hours is lower than or equal to ${queries.hours}.`);
                }
            } catch (e) {
                console.error(`query failed: ${e}`);
            } finally {
                await exit();
            }
        },
    ];
    // take out args
    const question: number = program['question'];
    const params: any = program['params'];
    // validate question id
    if (!isInteger(question) || question < 1 || question > actions.length) {
        console.error(`your question id is ${question + 1}, but should be 1-${actions.length}.`);
        await exit();
    }
    // validate params (basically)
    if (isNil(params)) {
        console.error(`your pass nil as params, should be a json string`);
        await exit();
    }
    await actions[question - 1](params);
}

main().then(() => {

});

